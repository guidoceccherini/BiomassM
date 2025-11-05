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



# Process each country folder
for(folder in country_folders) {
  
  country_name <- basename(folder)
  cat("\nProcessing:", country_name, "\n")
  
  # Find the disturbances and forest mask files
   # Define output paths
  output_dist <- file.path(folder, "disturbance_binary_2011_2023.tif")
  output_undist <- file.path(folder, "undisturbed.tif")
  output_agent <- file.path(folder, "agent_2011_2023.tif")
  
 
  
  # Store file paths for VRT creation
  disturbance_binary_files <- c(disturbance_binary_files, output_dist)
  undisturbed_files <- c(undisturbed_files, output_undist)
  agent_files <- c(agent_files, output_agent)
  
  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))
  cat(sprintf("  ✓ Saved: %s\n", basename(output_undist)))
  
  # Clean up memory
 
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")



# Create VRTs for EU-wide datasets
cat("\nCreating EU-wide virtual rasters...\n")

# VRT for disturbance binary 2011-2023
disturbance_eu_vrt <- vrt(unlist(disturbance_binary_files), 
                          "Data/disturbance_binary_2011_2023_EU.vrt", 
                          overwrite = TRUE)

# VRT for undisturbed forests
undisturbed_eu_vrt <- vrt(unlist(undisturbed_files), 
                          "Data/undisturbed_EU.vrt", 
                          overwrite = TRUE)

# VRT for agent driverst forests
agent_eu_vrt <- vrt(unlist(agent_files), 
                          "Data/agent_EU.vrt", 
                          overwrite = TRUE)


cat("✓ Created: disturbance_binary_2011_2023_EU.vrt\n")
cat("✓ Created: undisturbed_EU.vrt\n")

# Check VRTs
cat("\nDisturbance VRT info:\n")
print(disturbance_eu_vrt)

cat("\nUndisturbed VRT info:\n")
print(undisturbed_eu_vrt)



library(sf)
library(data.table)

# Load 
biomass_vrt <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc')
forest_type_vrt <- rast('Data/FTY_2015_100m_eu_03035_d02_Full/FTY_2015_100m_eu_03035_d02_full.tif')

biomass_vrt <- biomass_vrt[[1]]

EU_extent <- ext(disturbance_eu_vrt)
EU_crs <- crs(disturbance_eu_vrt)


# EEA_forest_type_cropped <- crop(forest_type_vrt, EU_extent)
# rcl_matrix <- matrix(c(
#   0, NA,    # all non-tree and non-forest areas -> NA (or use 0 if you want to keep it)
#   1, 1,     # broadleaved forest -> 1 (Broadleaf)
#   2, 2,     # coniferous forest -> 2 (Needleleaf)
#   3, 3,     # mixed forest -> 3 (Mixed Forest)
#   254, NA,  # unclassifiable -> NA
#   255, NA   # outside area -> NA
# ), ncol = 2, byrow = TRUE)
# 
# EEA_forest_type_cropped <- classify(EEA_forest_type_cropped, rcl_matrix)
# 
# terra::writeRaster(EEA_forest_type_cropped,
#             "Data/EEA_forest_type_cropped_EU.tif",
#             overwrite = TRUE,
#             gdal = c("COMPRESS=LZW", "TILED=YES"))

EEA_forest_type_cropped <- rast("Data/EEA_forest_type_cropped_EU.tif")

# check extents


# Per biomass con ricampionamento bilinear
# Estrai i parametri come valori numerici singoli
ref_ext <- ext(disturbance_eu_vrt)
ref_res <- res(disturbance_eu_vrt)
ref_crs <- crs(disturbance_eu_vrt, proj=TRUE)
# sf::gdal_utils(
#   util = "warp",
#   source = 'NETCDF:"Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc":agb',  # sostituisci 'agb' con il nome corretto
#   destination = "Data/Biomass2010_alignedEU.tif",
#   options = c(
#     "-t_srs", crs(disturbance_eu_vrt, proj=TRUE),
#     "-te", sprintf("%.6f", ref_ext[1]), sprintf("%.6f", ref_ext[3]),
#     sprintf("%.6f", ref_ext[2]), sprintf("%.6f", ref_ext[4]),
#     "-tr", sprintf("%.6f", ref_res[1]), sprintf("%.6f", ref_res[2]),
#     "-r", "bilinear",
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-co", "COMPRESS=LZW",
#     "-co", "TILED=YES",
#     "-co", "BIGTIFF=YES"
#   )
# )
Biomass2010_aligned <- rast("Data/Biomass2010_alignedEU.tif")


ref_ext <- as.vector(ext(disturbance_eu_vrt))
ref_res <- res(disturbance_eu_vrt)

# sf::gdal_utils(
#   util = "warp",
#   source = "Data/EEA_forest_type_cropped_EU.tif",  # modifica il path se necessario
#   destination = "Data/EEA_forest_type_alignedEU.tif",
#   options = c(
#     "-t_srs", crs(disturbance_eu_vrt, proj=TRUE),
#     "-te", sprintf("%.6f", ref_ext[1]), sprintf("%.6f", ref_ext[3]),
#     sprintf("%.6f", ref_ext[2]), sprintf("%.6f", ref_ext[4]),
#     "-tr", sprintf("%.6f", ref_res[1]), sprintf("%.6f", ref_res[2]),
#     "-r", "near",  # nearest neighbor per dati categorici
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-co", "COMPRESS=LZW",
#     "-co", "TILED=YES",
#     "-co", "BIGTIFF=YES"
#   )
# )

# Ricarica i risultati in terra
EEA_forest_type_aligned <- rast("Data/EEA_forest_type_alignedEU.tif")
# 
# Biomass2010_reprojected <- project(biomass_vrt[[1]],
#                                    disturbance_eu_vrt,
#                                    method = "bilinear")

# If EEA_forest_type is also in different CRS, reproject it
# EEA_forest_type_reprojected <- project(EEA_forest_type_cropped,
#                                        disturbance_eu_vrt,
#                                        method = "near")  # Use "near" for categorical data

# Step 2: Resample to match exact grid alignment
# This ensures pixels align perfectly
# Biomass2010_aligned <- resample(Biomass2010_reprojected,
#                                 disturbance_binary,
#                                 method = "bilinear")
# 
# EEA_forest_type_aligned <- resample(EEA_forest_type_reprojected,
#                                     disturbance_binary,
#                                     method = "near")

# Verify they now match
compareGeom(disturbance_eu_vrt, Biomass2010_aligned, EEA_forest_type_aligned)






# Stack all together
eu_stack <- c(Biomass2010_aligned, undisturbed_eu_vrt, EEA_forest_type_aligned)
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

# # Extract function
# extract_hex_data <- function(hex_idx, hex_geom, raster_stack) {
#   tryCatch({
#     hex_vals <- extract(raster_stack, hex_geom, ID = FALSE)
#     dt <- data.table(hex_vals)
#     
#     # Filter: only undisturbed pixels with valid forest type and biomass
#     dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
#                !is.na(biomass) & biomass > 0 &
#                !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
#     
#     if(nrow(dt) > 0) {
#       dt[, hex_ID := hex_idx]
#       return(dt)
#     } else {
#       return(NULL)
#     }
#   }, error = function(e) {
#     return(NULL)
#   })
# }
# 
# # Process all hexagons
# cat("Processing", nrow(Hex_EU), "hexagons across EU...\n")
# 
# all_results <- vector("list", nrow(Hex_EU))
# 
# for(i in 1:nrow(Hex_EU)) {
#   if(i %% 1000 == 0) {
#     cat(sprintf("[%s] Hexagon %d/%d (%.1f%%)\n", 
#                 Sys.time(), i, nrow(Hex_EU), 100*i/nrow(Hex_EU)))
#     gc()
#   }
#   
#   all_results[[i]] <- extract_hex_data(Hex_EU$hex_ID[i], 
#                                        Hex_EU[i, ], 
#                                        eu_stack)
# }
# 
# 
# 
# 
# # Combine results
# biomass_hex_dt <- rbindlist(all_results[!sapply(all_results, is.null)])
# 
# # Add forest type labels
# biomass_hex_dt[, forest_type_label := fcase(
#   forest_type == 1, "Broadleaf",
#   forest_type == 2, "Needleleaf",
#   forest_type == 3, "Mixed Forest",
#   default = NA_character_
# )]
# 
# # Save
# fwrite(biomass_hex_dt, "Data/biomass_EU_by_hexagon_undisturbed.csv")
# 
# cat("\nTotal valid pixels:", nrow(biomass_hex_dt), "\n")
# cat("Hexagons with data:", length(unique(biomass_hex_dt$hex_ID)), "\n")
# 
# 


# alternative for ram with cropping
library(exactextractr)

library(e1071)     # for skewness, kurtosis
library(nortest) 
# 
# chunk_size <- 1000
# n_chunks <- ceiling(nrow(Hex_EU) / chunk_size)
# all_results <- list()
# 
# 
# for(i in 1:n_chunks) {
#   cat(sprintf("[%s] Chunk %d/%d\n", Sys.time(), i, n_chunks))
#   
#   start_idx <- (i-1) * chunk_size + 1
#   end_idx <- min(i * chunk_size, nrow(Hex_EU))
#   hex_chunk <- Hex_EU[start_idx:end_idx, ]
#   
#   chunk_bbox <- st_bbox(hex_chunk)
#   chunk_ext <- ext(chunk_bbox[c("xmin", "xmax", "ymin", "ymax")])
#   buffer <- 10000
#   chunk_ext_buffered <- chunk_ext + buffer
#   eu_stack_cropped <- crop(eu_stack, chunk_ext_buffered)
#   
#   # Use a function that preserves hex_ID from include_cols
#   chunk_result <- exact_extract(eu_stack_cropped, hex_chunk, 
#                                 fun = function(df) {
#                                   dt <- as.data.table(df)
#                                   
#                                   # hex_ID is included in df due to include_cols
#                                   hex_id <- dt$hex_ID[1]  # All rows have same hex_ID
#                                   
#                                   dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
#                                              !is.na(biomass) & biomass > 0 &
#                                              !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
#                                   
#                                   if(nrow(dt) == 0) return(data.table())
#                                   
#                                   stats <- dt[, .(
#                                     mean_biomass = mean(biomass),
#                                     median_biomass = median(biomass),
#                                     q25_biomass = quantile(biomass, 0.25),
#                                     q75_biomass = quantile(biomass, 0.75),
#                                     n_pixels = .N
#                                   ), by = .(forest_type, drivers)]
#                                   
#                                   stats[, hex_ID := hex_id]
#                                   return(stats)
#                                 },
#                                 include_cols = "hex_ID",
#                                 summarize_df = TRUE,
#                                 max_cells_in_memory = 5e7)
#   
#   all_results[[i]] <- chunk_result
#   rm(eu_stack_cropped, chunk_result)
#   gc()
# }
# 
# final_data <- rbindlist(all_results)
# 
# write_csv(final_data, "Data/biomass_EU_by_hexagon_undisturbed_vNoHarvest.csv")
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
        # Compute distribution moments
        m <- mean(biomass)
        v <- var(biomass)
        s <- skewness(biomass, na.rm = TRUE, type = 2)
        k <- kurtosis(biomass, na.rm = TRUE, type = 2)
        
        # Anderson–Darling test for normality (fallback if too few points)
        p_norm <- if (.N >= 8) nortest::ad.test(biomass)$p.value else NA_real_
        
        .(mean_biomass = m,
          median_biomass = median(biomass),
          q5_biomass = quantile(biomass, 0.05),
          q95_biomass = quantile(biomass, 0.95),
          q25_biomass = quantile(biomass, 0.25),
          q75_biomass = quantile(biomass, 0.75),
          var_biomass = v,
          skew_biomass = s,
          kurt_biomass = k,
          p_normality = p_norm,
          n_pixels = .N)
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

final_data <- rbindlist(all_results)

write_csv(final_data, "Data/biomass_EU_by_hexagon_undisturbed_vNoHarvestDistributions.csv")




# same for disturbed
eu_stack <- c(Biomass2010_aligned, disturbance_eu_vrt, EEA_forest_type_aligned, agent_eu_vrt)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type", "drivers")

# chunk_size <- 1000
# n_chunks <- ceiling(nrow(Hex_EU) / chunk_size)
# all_results <- list()

# 
# for(i in 1:n_chunks) {
#   cat(sprintf("[%s] Chunk %d/%d\n", Sys.time(), i, n_chunks))
#   
#   start_idx <- (i-1) * chunk_size + 1
#   end_idx <- min(i * chunk_size, nrow(Hex_EU))
#   hex_chunk <- Hex_EU[start_idx:end_idx, ]
#   
#   chunk_bbox <- st_bbox(hex_chunk)
#   chunk_ext <- ext(chunk_bbox[c("xmin", "xmax", "ymin", "ymax")])
#   buffer <- 10000
#   chunk_ext_buffered <- chunk_ext + buffer
#   eu_stack_cropped <- crop(eu_stack, chunk_ext_buffered)
#   
#   # Use a function that preserves hex_ID from include_cols
#   chunk_result <- exact_extract(eu_stack_cropped, hex_chunk, 
#                                 fun = function(df) {
#                                   dt <- as.data.table(df)
#                                   
#                                   # hex_ID is included in df due to include_cols
#                                   hex_id <- dt$hex_ID[1]  # All rows have same hex_ID
#                                   
#                                   dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
#                                              !is.na(biomass) & biomass > 0 &
#                                              !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
#                                   
#                                   if(nrow(dt) == 0) return(data.table())
#                                   
#                                   stats <- dt[, .(
#                                     mean_biomass = mean(biomass),
#                                     median_biomass = median(biomass),
#                                     q25_biomass = quantile(biomass, 0.25),
#                                     q75_biomass = quantile(biomass, 0.75),
#                                     n_pixels = .N
#                                   ), by = .(forest_type, drivers)]
#                                   
#                                   stats[, hex_ID := hex_id]
#                                   return(stats)
#                                 },
#                                 include_cols = "hex_ID",
#                                 summarize_df = TRUE,
#                                 max_cells_in_memory = 5e7)
#   
#   all_results[[i]] <- chunk_result
#   rm(eu_stack_cropped, chunk_result)
#   gc()
# }
# 
# final_data <- rbindlist(all_results)
# 
# write_csv(final_data, "Data/biomass_EU_by_hexagon_disturbed_vNoHarvest.csv")
# 
# 
# 
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
                                  
                                  stats <- dt[, {
                                    # Compute distribution moments
                                    m <- mean(biomass)
                                    v <- var(biomass)
                                    s <- skewness(biomass, na.rm = TRUE, type = 2)
                                    k <- kurtosis(biomass, na.rm = TRUE, type = 2)
                                    
                                    # Anderson–Darling test for normality (fallback if too few points)
                                    p_norm <- if (.N >= 8) nortest::ad.test(biomass)$p.value else NA_real_
                                    
                                    .(mean_biomass = m,
                                      median_biomass = median(biomass),
                                      q5_biomass = quantile(biomass, 0.05),
                                      q95_biomass = quantile(biomass, 0.95),
                                      q25_biomass = quantile(biomass, 0.25),
                                      q75_biomass = quantile(biomass, 0.75),
                                      var_biomass = v,
                                      skew_biomass = s,
                                      kurt_biomass = k,
                                      p_normality = p_norm,
                                      n_pixels = .N)
                                  },by = .(forest_type)]
                                  
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

write_csv(final_data, "Data/biomass_EU_by_hexagon_disturbed_vNoHarvest2Distributions.csv")