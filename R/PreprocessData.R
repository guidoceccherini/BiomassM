library(terra)
library(tidyverse)
# Set your base directory containing all country folders

base_dir <- "/home/cecchgu/Downloads/13333034/"

# Get all country folders
country_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)

cat("Found", length(country_folders), "country folders\n")

# Initialize lists to store file paths
disturbance_binary_files <- list()
undisturbed_files <- list()

# Process each country folder
for(folder in country_folders) {
  
  country_name <- basename(folder)
  cat("\nProcessing:", country_name, "\n")
  
  # Find the disturbances and forest mask files
  dist_file <- list.files(folder, 
                          pattern = "annual_disturbances.*\\.tif$", 
                          full.names = TRUE)
  
  forest_file <- list.files(folder, 
                            pattern = "forest_mask.*\\.tif$", 
                            full.names = TRUE)
  
  # Check if files exist
  if(length(dist_file) == 0 || length(forest_file) == 0) {
    warning(sprintf("Missing files in %s - skipping", country_name))
    next
  }
  
  # Read rasters
  Disturbances <- rast(dist_file[1])
  ForestMask <- rast(forest_file[1])
  
  # Extract layers for 2011-2023 (layers 27-39, assuming 1985 is layer 1)
  disturbances_2011_2023 <- Disturbances[[27:39]]
  
  # Create binary raster for 2011-2023 disturbances
  disturbance_binary11_23 <- max(disturbances_2011_2023, na.rm = TRUE)
  disturbance_binary11_23 <- ifel(disturbance_binary11_23 > 0, 1, 0)
  
  # Create binary for ALL disturbances
  disturbance_binaryAll <- max(Disturbances, na.rm = TRUE)
  
  # Create undisturbed mask
  undisturbed <- ifel(disturbance_binaryAll > 0, NA, ForestMask)
  
  # Define output paths
  output_dist <- file.path(folder, "disturbance_binary_2011_2023.tif")
  output_undist <- file.path(folder, "undisturbed.tif")
  
  # Write outputs
  writeRaster(disturbance_binary11_23, 
              output_dist, 
              overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES"))
  
  writeRaster(undisturbed, 
              output_undist, 
              overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES"))
  
  # Store file paths for VRT creation
  disturbance_binary_files <- c(disturbance_binary_files, output_dist)
  undisturbed_files <- c(undisturbed_files, output_undist)
  
  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))
  cat(sprintf("  ✓ Saved: %s\n", basename(output_undist)))
  
  # Clean up memory
  rm(Disturbances, ForestMask, disturbances_2011_2023, 
     disturbance_binary11_23, disturbance_binaryAll, undisturbed)
  gc()
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")
