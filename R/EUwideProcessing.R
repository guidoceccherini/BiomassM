library(terra)
library(sf)
library(data.table)

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

cat("✓ Created: disturbance_binary_2011_2023_EU.vrt\n")
cat("✓ Created: undisturbed_EU.vrt\n")

# Check VRTs
cat("\nDisturbance VRT info:\n")
print(disturbance_eu_vrt)

cat("\nUndisturbed VRT info:\n")
print(undisturbed_eu_vrt)



library(sf)
library(data.table)

# Load other datasets as VRTs (biomass, forest type, etc.)
biomass_files <- list.files(base_dir, 
                            pattern = "biomass.*\\.tif$", 
                            recursive = TRUE, 
                            full.names = TRUE)

forest_type_files <- list.files(base_dir, 
                                pattern = "forest_type.*\\.tif$", 
                                recursive = TRUE, 
                                full.names = TRUE)

biomass_vrt <- vrt(biomass_files, "Data/biomass_EU.vrt", overwrite = TRUE)
forest_type_vrt <- vrt(forest_type_files, "Data/forest_type_EU.vrt", overwrite = TRUE)

# Stack all together
eu_stack <- c(biomass_vrt, undisturbed_eu_vrt, forest_type_vrt)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type")

# Load EU hexagon grid
Hex_EU <- st_read("Data/Hexs/Hex_EU.gpkg")

# Reproject if needed
if(st_crs(Hex_EU) != st_crs(eu_stack)) {
  Hex_EU <- st_transform(Hex_EU, st_crs(eu_stack))
}

# Add hex ID
if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}

# Extract function
extract_hex_data <- function(hex_idx, hex_geom, raster_stack) {
  tryCatch({
    hex_vals <- extract(raster_stack, hex_geom, ID = FALSE)
    dt <- data.table(hex_vals)
    
    # Filter: only undisturbed pixels with valid forest type and biomass
    dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
               !is.na(biomass) & biomass > 0 &
               !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
    
    if(nrow(dt) > 0) {
      dt[, hex_ID := hex_idx]
      return(dt)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Process all hexagons
cat("Processing", nrow(Hex_EU), "hexagons across EU...\n")

all_results <- vector("list", nrow(Hex_EU))

for(i in 1:nrow(Hex_EU)) {
  if(i %% 1000 == 0) {
    cat(sprintf("[%s] Hexagon %d/%d (%.1f%%)\n", 
                Sys.time(), i, nrow(Hex_EU), 100*i/nrow(Hex_EU)))
    gc()
  }
  
  all_results[[i]] <- extract_hex_data(Hex_EU$hex_ID[i], 
                                       Hex_EU[i, ], 
                                       eu_stack)
}

# Combine results
biomass_hex_dt <- rbindlist(all_results[!sapply(all_results, is.null)])

# Add forest type labels
biomass_hex_dt[, forest_type_label := fcase(
  forest_type == 1, "Broadleaf",
  forest_type == 2, "Needleleaf",
  forest_type == 3, "Mixed Forest",
  default = NA_character_
)]

# Save
fwrite(biomass_hex_dt, "Data/biomass_EU_by_hexagon_undisturbed.csv")

cat("\nTotal valid pixels:", nrow(biomass_hex_dt), "\n")
cat("Hexagons with data:", length(unique(biomass_hex_dt$hex_ID)), "\n")




# OPTION PARALLEL PROCESSING (uncomment to use)
# library(future.apply)
# plan(multisession, workers = 4)  # Use 4 cores
# 
# all_results <- future_lapply(1:nrow(Hex_EU), function(i) {
#   extract_hex_data(Hex_EU$hex_ID[i], Hex_EU[i, ], eu_stack)
# }, future.seed = TRUE)
