library(tidyverse)

base_dir <- "/home/cecchgu/Downloads/13333034/"
country_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)

for(folder in country_folders) {
  country_name <- basename(folder)
  cat("\nProcessing:", country_name, "\n")
  
  dist_file <- list.files(folder, pattern = "annual_disturbances.*\\.tif$", 
                          full.names = TRUE)
  forest_file <- list.files(folder, pattern = "forest_mask.*\\.tif$", 
                            full.names = TRUE)
  
  if(length(dist_file) == 0 || length(forest_file) == 0) {
    warning(sprintf("Missing files in %s - skipping", country_name))
    next
  }
  
  temp_dir <- file.path(folder, "temp_gdal")
  dir.create(temp_dir, showWarnings = FALSE)
  output_dist <- file.path(folder, "disturbance_binned_3year_2008_2019.tif")
  
  # Step 1: Extract individual bands 24-35 as separate TIFFs
  cat("  Extracting bands 24-35...\n")
  band_files <- character(12)
  for(i in 1:12) {
    band_num <- i + 23  # Layers 24-35
    band_files[i] <- file.path(temp_dir, sprintf("band_%02d.tif", i))
    
    system2("gdal_translate", 
            args = c(
              "-b", band_num,
              "-co", "COMPRESS=LZW",
              "-co", "TILED=YES",
              shQuote(dist_file[1]),
              shQuote(band_files[i])
            ),
            stdout = FALSE, stderr = FALSE)
  }
  
  # Step 2: Calculate max for each 3-year bin
  cat("  Computing bin maximums...\n")
  bin_files <- character(4)
  bins <- list(1:3, 4:6, 7:9, 10:12)
  
  for(bin_num in seq_along(bins)) {
    indices <- bins[[bin_num]]
    bin_files[bin_num] <- file.path(temp_dir, sprintf("bin_%d_max.tif", bin_num))
    
    if(length(indices) == 3) {
      calc_expr <- "maximum(maximum(A,B),C)"
      
      system2("gdal_calc.py",
              args = c(
                "-A", shQuote(band_files[indices[1]]),
                "-B", shQuote(band_files[indices[2]]),
                "-C", shQuote(band_files[indices[3]]),
                "--calc", shQuote(calc_expr),
                "--outfile", shQuote(bin_files[bin_num]),
                "--type", "Byte",
                "--co", "COMPRESS=LZW",
                "--co", "TILED=YES",
                "--hideNoData",
                "--overwrite",
                "--quiet"
              ),
              stdout = FALSE, stderr = FALSE)
      
    } else {  # Bin 4 with 2 bands
      calc_expr <- "maximum(A,B)"
      
      system2("gdal_calc.py",
              args = c(
                "-A", shQuote(band_files[indices[1]]),
                "-B", shQuote(band_files[indices[2]]),
                "--calc", shQuote(calc_expr),
                "--outfile", shQuote(bin_files[bin_num]),
                "--type", "Byte",
                "--co", "COMPRESS=LZW",
                "--co", "TILED=YES",
                "--hideNoData",
                "--overwrite",
                "--quiet"
              ),
              stdout = FALSE, stderr = FALSE)
    }
  }
  
  # Step 3: Create final binned raster WITH FOREST MASK
  cat("  Creating final output with forest mask...\n")
  
  # KEY FIX: Include forest mask (F) in the calculation
  # Where forest exists and disturbance occurred, assign bin number
  # Where forest doesn't exist (F==0 or F is NoData), assign 255 (NoData)
  calc_expr <- "where(F>0, where(A>0, 1, where(B>0, 2, where(C>0, 3, where(D>0, 4, 0)))), 255)"
  
  result <- system2("gdal_calc.py",
                    args = c(
                      "-A", shQuote(bin_files[1]),
                      "-B", shQuote(bin_files[2]),
                      "-C", shQuote(bin_files[3]),
                      "-D", shQuote(bin_files[4]),
                      "-F", shQuote(forest_file[1]),  # KEY: Add forest mask
                      "--calc", shQuote(calc_expr),
                      "--outfile", shQuote(output_dist),
                      "--type", "Byte",
                      "--co", "COMPRESS=LZW",
                      "--co", "TILED=YES",
                      "--co", "BIGTIFF=YES",
                      "--NoDataValue", "255",
                      "--overwrite"
                    ),
                    stdout = TRUE, stderr = TRUE)
  
  # Verify output
  if(file.exists(output_dist)) {
    cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))
    
    # Quick stats check
    info <- system2("gdalinfo", 
                    args = c("-stats", "-mm", shQuote(output_dist)), 
                    stdout = TRUE, stderr = FALSE)
    
    # Extract and display min/max values
    minmax_line <- grep("Computed Min/Max=", info, value = TRUE)
    if(length(minmax_line) > 0) {
      cat(sprintf("    %s\n", minmax_line[1]))
    }
  } else {
    warning("  ✗ Output file NOT created!")
    cat("Error output:\n")
    cat(result, sep = "\n")
  }
  
  # Clean up temporary files
  unlink(temp_dir, recursive = TRUE)
}

cat("\n✓ Processing complete!\n")
