library(tidyverse)
library(terra)
library(sf)
library(data.table)


base_dir <- "/home/cecchgu/Downloads/13333034/"

# Get all country folders
country_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)

cat("Found", length(country_folders), "country folders\n")

# Initialize lists to store file paths
disturbance_binary_files <- list()
undisturbed_files <- list()
agent_files <- list()
severity_files <- list()



# Process each country folder
for(folder in country_folders) {
  
  country_name <- basename(folder)
  cat("\nProcessing:", country_name, "\n")
  
  # Find the disturbances and forest mask files
   # Define output paths
  output_dist <- file.path(folder, "disturbance_binned_3year_2008_2019.tif")
  output_undist <- file.path(folder, "undisturbed.tif")
  output_agent <- file.path(folder, "agent_2008_2019.tif")
  output_severity <- file.path(folder, "severity_2008_2019.tif")
  
 
  
  # Store file paths for VRT creation
  disturbance_binary_files <- c(disturbance_binary_files, output_dist)
  undisturbed_files <- c(undisturbed_files, output_undist)
  agent_files <- c(agent_files, output_agent)
  severity_files <- c(severity_files, output_severity)
  
  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))
  cat(sprintf("  ✓ Saved: %s\n", basename(output_undist)))
  
  # Clean up memory
 
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")



# Create VRTs for EU-wide datasets
cat("\nCreating EU-wide virtual rasters...\n")

# VRT for disturbance binary 2011-2019

# Get all disturbance files
disturbance_files <- unlist(disturbance_binary_files)

# Build VRT using GDAL command with proper NA handling
vrt_output <- "Data/disturbance_bin_2008_2019_EU.vrt"

# Create a text file with all input files (GDAL requires this for many files)
file_list <- "temp_file_list.txt"
writeLines(disturbance_files, file_list)

# Use gdalbuildvrt with -hidenodata flag
system2("gdalbuildvrt",
        args = c(
          "-hidenodata",  # KEY: Makes nodata transparent in overlaps
          "-srcnodata", "0",  # Define source nodata value
          "-vrtnodata", "0",  # Define VRT nodata value
          "-input_file_list", file_list,  # List of input files
          vrt_output
        ),
        stdout = TRUE, stderr = TRUE)

# Now read and convert to final raster if needed
disturbance_eu <- rast(vrt_output)

# Optional: write to permanent GeoTIFF
# writeRaster(disturbance_eu, 
#             "Data/disturbance_bin_2008_2019_EU.tif",
#             overwrite = TRUE,
#             gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))

# Clean up
unlink(file_list)






# VRT for undisturbed forests
undisturbed_eu_vrt <- vrt(unlist(undisturbed_files), 
                          "Data/undisturbed_EU.vrt", 
                          overwrite = TRUE)

# VRT for agent driverst forests
agent_eu_vrt <- vrt(unlist(agent_files), 
                          "Data/agent_EU.vrt", 
                          overwrite = TRUE)

# VRT for agent severity forests
severity_eu_vrt <- vrt(unlist(severity_files), 
                    "Data/severity_EU.vrt", 
                    overwrite = TRUE)


cat("✓ Created: disturbance_binary_2011_2019_EU.vrt\n")
cat("✓ Created: undisturbed_EU.vrt\n")

# Check VRTs
cat("\nDisturbance VRT info:\n")
print(disturbance_eu_vrt)

cat("\nUndisturbed VRT info:\n")
print(undisturbed_eu_vrt)





library(sf)
library(data.table)

# Load 
biomass_vrt <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv6.0.nc')
forest_type_vrt <- rast('Data/FTY_2015_100m_eu_03035_d02_Full/FTY_2015_100m_eu_03035_d02_full.tif')

biomass_vrt <- biomass_vrt[[1]]



# Load rasters
EEA_forest_type_cropped <- rast("Data/EEA_forest_type_cropped_EU.tif")

# Create custom aggregation function
threshold_75_fun <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  prop_ones <- sum(x == 1) / length(x)
  ifelse(prop_ones >= 0.75, 1, 0)
}

# Directly resample using disaggregate-aggregate workflow
# This ensures proper alignment with the reference raster
# disturbance_100m <- resample(disturbance_eu_vrt, 
#                              EEA_forest_type_cropped,
#                              method = "average")
# 
# # Apply threshold: keep only pixels where average >= 0.75 (meaning 75% are 1s)
# disturbance_100m <- ifel(disturbance_100m >= 0.75, 1, 0)
# 
# # Save result
# writeRaster(disturbance_100m, "Data/disturbance_100m_threshold75.tif", overwrite = TRUE)
# disturbance_100m <- rast("Data/disturbance_100m_threshold75.tif")
# Calculate aggregation factor (30m to 100m = ~3.33x)
agg_factor <- 100 / 30  # ~3.33, round to 3

# Define a custom function to check modal purity
modal_with_threshold <- function(x, threshold = 0.75) {
  # Remove NA values
  x <- x[!is.na(x)]
  
  if(length(x) == 0) return(NA)
  
  # Find the mode (most common value)
  tbl <- table(x)
  modal_value <- as.numeric(names(tbl)[which.max(tbl)])
  modal_count <- max(tbl)
  
  # Calculate purity (proportion of modal value)
  purity <- modal_count / length(x)
  
  # Return modal value only if purity >= threshold, else 0 or NA
  if(purity >= threshold) {
    return(modal_value)
  } else {
    return(0)  # Or use NA if you prefer
  }
}

# Aggregate with purity check
disturbance_100m <- aggregate(disturbance_eu_vrt, 
                              fact = round(agg_factor),
                              fun = function(x) modal_with_threshold(x, threshold = 0.75))
                              # na.rm = TRUE)

# Then resample to exact alignment if needed
disturbance_100m <- resample(disturbance_100m, 
                             EEA_forest_type_cropped,
                             method = "near")  # Nearest neighbor to preserve values

# same for undisturbed
# undisturbance_100m <- resample(undisturbed_eu_vrt, 
#                              EEA_forest_type_cropped,
#                              method = "average")
# 
# # Apply threshold: keep only pixels where average >= 0.75 (meaning 75% are 1s)
# undisturbance_100m <- ifel(undisturbance_100m >= 0.75, 1, 0)
# 
# # Save result
# writeRaster(undisturbance_100m, "Data/undisturbance_100m_threshold75.tif", overwrite = TRUE)
undisturbance_100m <- rast("Data/undisturbance_100m_threshold75.tif")

# same with agent, but in this case use the mode

agent_eu_vrt_100m <- resample(agent_eu_vrt,
                              EEA_forest_type_cropped,
                              method = "mode")

writeRaster(agent_eu_vrt_100m, "Data/agent_eu_vrt_100m.tif", overwrite = TRUE)
agent_eu_vrt_100m <- rast( "Data/agent_eu_vrt_100m.tif")
# same wwith severity

severity_100m <- resample(severity_eu_vrt,
                               EEA_forest_type_cropped,
                               method = "average")

# writeRaster(severity_100m, "Data/severity_100m.tif", overwrite = TRUE)
severity_100m <- rast("Data/severity_100m.tif")



# EU_extent <- ext(disturbance_eu_vrt)
# EU_crs <- crs(disturbance_eu_vrt)
# biomass_reprojected <- project(biomass_vrt, 
#                                EEA_forest_type_cropped, 
#                                method = "bilinear")
# 
# # Check the result
# biomass_reprojected


# library(terra)

# Using gdalwarp through sf::gdal_utils
# sf::gdal_utils(
#   util = "warp",
#   source = 'NETCDF:"Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv6.0.nc":agb',
#   destination = "Data/biomass_LAEA_100m.tif",
#   options = c(
#     "-t_srs", "EPSG:3035",                    # Target CRS
#     "-tr", "100", "100",                       # Target resolution (100m x 100m)
#     "-te", "2570500", "1388800", "6549600", "5436400",  # Target extent (from reference)
#     "-r", "bilinear",                          # Resampling method
#     "-of", "GTiff",                            # Output format
#     "-co", "COMPRESS=LZW"                      # Compression
#   )
# )

# Load the result
biomass_reprojected <- rast("Data/biomass_LAEA_100m.tif")



# 
# 
# 
# # Per biomass con ricampionamento bilinear
# # Estrai i parametri come valori numerici singoli
# ref_ext <- ext(disturbance_eu_vrt)
# ref_res <- res(disturbance_eu_vrt)
# ref_crs <- crs(disturbance_eu_vrt, proj=TRUE)
# # sf::gdal_utils(
# #   util = "warp",
# #   source = 'NETCDF:"Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc":agb',  # sostituisci 'agb' con il nome corretto
# #   destination = "Data/Biomass2010_alignedEU.tif",
# #   options = c(
# #     "-t_srs", crs(disturbance_eu_vrt, proj=TRUE),
# #     "-te", sprintf("%.6f", ref_ext[1]), sprintf("%.6f", ref_ext[3]),
# #     sprintf("%.6f", ref_ext[2]), sprintf("%.6f", ref_ext[4]),
# #     "-tr", sprintf("%.6f", ref_res[1]), sprintf("%.6f", ref_res[2]),
# #     "-r", "bilinear",
# #     "-multi",
# #     "-wo", "NUM_THREADS=ALL_CPUS",
# #     "-co", "COMPRESS=LZW",
# #     "-co", "TILED=YES",
# #     "-co", "BIGTIFF=YES"
# #   )
# # )
# Biomass2010_aligned <- rast("Data/Biomass2010_alignedEU.tif")

# Verify they now match
compareGeom(disturbance_100m, biomass_reprojected, EEA_forest_type_cropped)

compareGeom(undisturbance_100m, biomass_reprojected, EEA_forest_type_cropped)





# Stack all together
eu_stack <- c(biomass_reprojected, undisturbance_100m, EEA_forest_type_cropped)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type" )

# Load EU hexagon grid
Hex_EU <- st_read("Data/grid_forest.gpkg")

Hex_EU <- Hex_EU|> dplyr::filter(forest_count >0)

# Reproject if needed
if(st_crs(Hex_EU) != st_crs(eu_stack)) {
  Hex_EU <- st_transform(Hex_EU, st_crs(eu_stack))
}

# Add hex ID
if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}



# alternative for ram with cropping
library(exactextractr)

library(e1071)     # for skewness, kurtosis
library(nortest) 
# 
# 
# 

# same for disturbed
eu_stack <- c(biomass_reprojected, disturbance_100m, EEA_forest_type_cropped, agent_eu_vrt_100m, severity_100m)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type", "drivers", "severity" )

# 
chunk_size <- 1000
n_chunks <- ceiling(nrow(Hex_EU) / chunk_size)
all_results <- list()


for(i in 1:n_chunks) {
  cat(sprintf("[%s] Chunk %d/%d\n", Sys.time(), i, n_chunks))
  
  start_idx <- (i-1) * chunk_size + 1
  end_idx <- min(i * chunk_size, nrow(Hex_EU))
  hex_chunk <- Hex_EU[start_idx:end_idx, ]
  
  chunk_bbox <- st_bbox(hex_chunk)
  chunk_ext <- ext(chunk_bbox[c("xmin", "xmax", "ymin", "ymax")])
  buffer <- 10000
  chunk_ext_buffered <- chunk_ext + buffer
  eu_stack_cropped <- crop(eu_stack, chunk_ext_buffered)
  
  # Use a function that preserves hex_ID from include_cols
  chunk_result <- exact_extract(eu_stack_cropped, hex_chunk, 
                                fun = function(df) {
                                  dt <- as.data.table(df)
                                  
                                  # hex_ID is included in df due to include_cols
                                  hex_id <- dt$hex_ID[1]  # All rows have same hex_ID
                                  
                                  dt <- dt[!is.na(undisturbed) & undisturbed >= 1 &
                                             !is.na(severity) & severity < 10 &
                                             !is.na(biomass) & biomass > 0 &
                                             !is.na(forest_type) & forest_type %in% c(1, 2, 3)&
                                             !is.na(drivers) & drivers %in% c(1, 2)]
                                  
                                  if(nrow(dt) == 0) return(data.table())
                                  
                                  
                                  dt[, disturbed_bin := fifelse(undisturbed == 1, "1",
                                                               fifelse(undisturbed == 2, "2",
                                                                       fifelse(undisturbed == 3, "3",
                                                                               fifelse(undisturbed == 4, "4","X"))))]
                                  
                                  stats <- dt[, {
                                    n <- .N
                                    
                                    # Compute only if enough observations
                                    if (n >= 3) {
                                      m <- mean(biomass)
                                      v <- var(biomass)
                                      s <- tryCatch(e1071::skewness(biomass, na.rm = TRUE, type = 2), error = function(e) NA_real_)
                                      k <- tryCatch(e1071::kurtosis(biomass, na.rm = TRUE, type = 2), error = function(e) NA_real_)
                                    } else {
                                      m <- v <- s <- k <- NA_real_
                                    }
                                    
                                    # Normality test only if enough samples
                                    p_norm <- if (n >= 8) tryCatch(nortest::ad.test(biomass)$p.value, error = function(e) NA_real_) else NA_real_
                                    
                                    .(mean_biomass = m,
                                      median_biomass = median(biomass),
                                      q25_biomass = quantile(biomass, 0.25),
                                      q75_biomass = quantile(biomass, 0.75),
                                      var_biomass = v,
                                      skew_biomass = s,
                                      kurt_biomass = k,
                                      p_normality = p_norm,
                                      n_pixels = n)
                                  }, by = .(forest_type, disturbed_bin)]
                                  
                                  stats[, hex_ID := hex_id]
                                  return(stats)
                                },
                                include_cols = "hex_ID",
                                summarize_df = TRUE,
                                max_cells_in_memory = 5e7)
  
  all_results[[i]] <- chunk_result
  rm(eu_stack_cropped, chunk_result)
  gc()
}

final_data <- rbindlist(all_results)

write_csv(final_data, "Data/biomass_EU_2020Time_NoHarvest2DistributionSeverity.csv")




eu_stack <- c(biomass_reprojected, undisturbance_100m, EEA_forest_type_cropped)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type" )



chunk_size <- 1000
n_chunks <- ceiling(nrow(Hex_EU) / chunk_size)
all_results <- list()


for(i in 1:n_chunks) {
  cat(sprintf("[%s] Chunk %d/%d\n", Sys.time(), i, n_chunks))
  
  start_idx <- (i-1) * chunk_size + 1
  end_idx <- min(i * chunk_size, nrow(Hex_EU))
  hex_chunk <- Hex_EU[start_idx:end_idx, ]
  
  chunk_bbox <- st_bbox(hex_chunk)
  chunk_ext <- ext(chunk_bbox[c("xmin", "xmax", "ymin", "ymax")])
  buffer <- 10000
  chunk_ext_buffered <- chunk_ext + buffer
  eu_stack_cropped <- crop(eu_stack, chunk_ext_buffered)
  
  chunk_result <- exact_extract(
    eu_stack_cropped, hex_chunk,
    fun = function(df) {
      dt <- as.data.table(df)
      hex_id <- dt$hex_ID[1]
      
      dt <- dt[
        !is.na(undisturbed) & undisturbed == 1 &
          !is.na(biomass) & biomass > 0 &
          !is.na(forest_type) & forest_type %in% c(1, 2, 3)
      ]
      if (nrow(dt) == 0) return(data.table())
      
      stats <- dt[, {
        n <- .N
        
        # Compute only if enough observations
        if (n >= 3) {
          m <- mean(biomass)
          v <- var(biomass)
          s <- tryCatch(e1071::skewness(biomass, na.rm = TRUE, type = 2), error = function(e) NA_real_)
          k <- tryCatch(e1071::kurtosis(biomass, na.rm = TRUE, type = 2), error = function(e) NA_real_)
        } else {
          m <- v <- s <- k <- NA_real_
        }
        
        # Normality test only if enough samples
        p_norm <- if (n >= 8) tryCatch(nortest::ad.test(biomass)$p.value, error = function(e) NA_real_) else NA_real_
        
        .(mean_biomass = m,
          median_biomass = median(biomass),
          q25_biomass = quantile(biomass, 0.25),
          q75_biomass = quantile(biomass, 0.75),
          var_biomass = v,
          skew_biomass = s,
          kurt_biomass = k,
          p_normality = p_norm,
          n_pixels = n)
      }, by = forest_type]
      
      stats[, hex_ID := hex_id]
      stats
    },
    include_cols = "hex_ID",
    summarize_df = TRUE,
    max_cells_in_memory = 5e7
  )
  
  all_results[[i]] <- chunk_result
  rm(eu_stack_cropped, chunk_result)
  gc()
}

final_dataUND <- rbindlist(all_results)
write_csv(final_dataUND, "Data/biomass_EU_2020Timeby_hexagon_undisturbed_vNoHarvestDistributions.csv")
