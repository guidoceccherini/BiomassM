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
  agent_file <- list.files(folder, 
                          pattern = "disturbance_agent_1985_2019.*\\.tif$", 
                          full.names = TRUE)
  
  disturbance_binary_file <- list.files(folder,
                            pattern = "disturbance_binary_2011_2019.tif",
                            full.names = TRUE)

  # Check if files exist
  if(length(agent_file) == 0 ) {
    warning(sprintf("Missing files in %s - skipping", country_name))
    next
  }
  
  # Read rasters
  agent <- rast(agent_file[1])
  disturbance_binary <- rast(disturbance_binary_file)
  
  # Extract layers for 2011-2019 (layers 27-39, assuming 1985 is layer 1)
  agent_2011_2019 <- agent[[27:35]]
  
 
  mask <- disturbance_binary == 1

  # OPTION A: Get the maximum agent value across the 2015-2020 period
  agents_max <- app(agent_2011_2019, max, na.rm = TRUE)

  # Apply the binary mask: set cells to NA where binary = 0
  result_raster <- agents_max
  result_raster[!mask] <- NA
  
  
  
  
  
  # Define output paths
  output_dist <- file.path(folder, "agent_2011_2019.tif")

  # Write outputs
  writeRaster(result_raster, 
              output_dist, 
              overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES"))
  
  
  # Store file paths for VRT creation
  agent_binary_files <- c(output_dist)

  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))

  # Clean up memory
  rm(Disturbances, result_raster, mask, 
     agent_2011_2019, agents_max, disturbance_binary)
  gc()
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")
