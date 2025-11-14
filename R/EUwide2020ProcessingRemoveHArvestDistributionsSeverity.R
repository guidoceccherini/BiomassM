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
  output_dist <- file.path(folder, "disturbance_binary_2011_2019.tif")
  output_undist <- file.path(folder, "undisturbed.tif")
  output_agent <- file.path(folder, "agent_2011_2019.tif")
  output_severity <- file.path(folder, "severity_2011_2019.tif")
  
 
  
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
disturbance_eu_vrt <- vrt(unlist(disturbance_binary_files), 
                          "Data/disturbance_binary_2011_2019_EU.vrt", 
                          overwrite = TRUE)

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
disturbance_100m <- rast("Data/disturbance_100m_threshold75.tif")


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

# agent_eu_vrt_100m <- resample(agent_eu_vrt, 
#                               EEA_forest_type_cropped,
#                               method = "mode")
# 
# writeRaster(agent_eu_vrt_100m, "Data/agent_eu_vrt_100m.tif", overwrite = TRUE)
agent_eu_vrt_100m <- rast( "Data/agent_eu_vrt_100m.tif")
# same wwith severity

# severity_100m <- resample(severity_eu_vrt, 
#                                EEA_forest_type_cropped,
#                                method = "average")
# 
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
                                  
                                  dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
                                             !is.na(biomass) & biomass > 0 &
                                             !is.na(forest_type) & forest_type %in% c(1, 2, 3)&
                                             !is.na(drivers) & drivers %in% c(1, 2)]
                                  
                                  if(nrow(dt) == 0) return(data.table())
                                  
                                  
                                  dt[, severity_bin := fifelse(severity %in% 1:5, "1-5",
                                                               fifelse(severity == 6, "6",
                                                                       fifelse(severity == 7, "7",
                                                                               fifelse(severity == 8, "8",
                                                                                       fifelse(severity == 9, "9", "10-11")))))]
                                  
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
                                  }, by = .(forest_type, severity_bin)]
                                  
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

write_csv(final_data, "Data/biomass_EU_2020_NoHarvest2DistributionSeverity.csv")




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
write_csv(final_dataUND, "Data/biomass_EU_2020by_hexagon_undisturbed_vNoHarvestDistributions.csv")
