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
  severity_file <- list.files(folder, 
                          pattern = "disturbance_severity_1985_2023.*\\.tif$", 
                          full.names = TRUE)
  
  disturbance_binary_file <- list.files(folder,
                            pattern = "disturbance_binary_2011_2019.tif",
                            full.names = TRUE)

  # Check if files exist
  if(length(severity_file) == 0 ) {
    warning(sprintf("Missing files in %s - skipping", country_name))
    next
  }
  
  # Read rasters
  severity <- rast(severity_file[1])
  disturbance_binary <- rast(disturbance_binary_file)
  
  # Extract layers for 2011-2019 (layers 27-39, assuming 1985 is layer 1)
  severity_2011_2019 <- severity[[27:35]]
  
 
  mask <- disturbance_binary == 1
  
  

  # OPTION A: Get the maximum agent value across the 2015-2020 period
  severity_min <- app(severity_2011_2019, min, na.rm = TRUE)

  # Apply the binary mask: set cells to NA where binary = 0
  result_raster <- severity_min
  result_raster[!mask] <- NA
  
  
  # Define class breaks
  breaks <- c(-50000, seq(-9000, 0, by = 1000), 50000)  # 12 edges → 11 bins
  
  # Create a reclassification matrix: from, to, becomes
  rcl <- cbind(breaks[-length(breaks)], breaks[-1], 1:11)
  
  # Apply reclassification to all layers
  r_binned <- classify(result_raster, rcl = rcl, include.lowest = TRUE, right = TRUE)
  
  
  
  
  # Define output paths
  output_dist <- file.path(folder, "severity_2011_2019.tif")

  # Write outputs
  writeRaster(r_binned, 
              output_dist, 
              overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES"))
  
  
  # Store file paths for VRT creation
  agent_binary_files <- c(output_dist)

  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))

  # Clean up memory
  rm(Disturbances, r_binned, mask,  severity,
     severity_2011_2019, severity_min, disturbance_binary)
  gc()
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")
