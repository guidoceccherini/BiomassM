library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(ggplot2)
library("stringr")
library(scales)
library(ggspatial)
library(colorblindcheck)
library(rcartocolor)
library(cols4all)
library(ggpubfigs)
library(tidyverse)
library(jsonlite)
library(terra)

# usare Biogeoregioni
# usare atlas of disturbances di Senf: https://zenodo.org/records/13333034 
# Usa Esagoni di Daniele
# usa forest type mask da EEA
# usa biomassa di esa cci per 2010 e 2020 e il loro relativo cambio: https://catalogue.ceda.ac.uk/uuid/95913ffb6467447ca72c4e9d8cf30501/  o ftp: ftp://anon-ftp.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v6.0 
# Prendi i pixel per cui hai loss dal 2015 al 2020 (salta 2010-15 per evitare problemi di ricresicita) e fai delle distribuzioni di frequenza di agb loss per nnedleleaf e broadleaf per aree geografiche



# open Biogeoregions

BG <- st_read('Data/BiogeoRegions2016.shp')

# open hexagons

Hex <- st_read('Data/grid_forest.gpkg')


# open Senf dataset of disturbances

Disturbances <- rast('Data/czechia/annual_disturbances_1985_2023_czechia.tif')

# # Identify which layers correspond to 2015-2020
# # Assuming layer 1 = 1985, layer 2 = 1986, etc.
# # 2015 would be layer 31 (1985 + 30 = 2015)
# # 2020 would be layer 36 (1985 + 35 = 2020)

# # Extract layers for 2015-2020 (layers 31-36)
disturbances_2015_2020 <- Disturbances[[31:36]]
disturbances_2011_2023 <- Disturbances[[27:39]]

# # Check layer names to confirm
# names(disturbances_2015_2020)

# # Create binary raster: 1 if ANY disturbance in 2015-2020, 0 otherwise
# # # Method 1: Using max() - if any layer has a value > 0
disturbance_binary <- max(disturbances_2015_2020, na.rm = TRUE)
disturbance_binary11_23 <- max(disturbances_2011_2023, na.rm = TRUE)
disturbance_binaryAll <- max(Disturbances, na.rm = TRUE)
# # Convert to binary (1 if disturbed, 0 if not)
disturbance_binary <- ifel(disturbance_binary > 0, 1, 0)
disturbance_binary11_23 <- ifel(disturbance_binary11_23 > 0, 1, 0)

# # Check result
# disturbance_binary
plot(disturbance_binary, main = "Disturbances 2015-2020 (1 = disturbed)")
plot(disturbance_binary11_23, main = "Disturbances 2011-2023 (1 = disturbed)")


# # Save the output
writeRaster(disturbance_binary,
            "Data/czechia/disturbance_binary_2015_2020.tif",
            overwrite = TRUE)

writeRaster(disturbance_binary11_23,
            "Data/czechia/disturbance_binary_2011_2023.tif",
            overwrite = TRUE)


# undisturbed pixels
ForestMask <- rast('Data/czechia/forest_mask_czechia.tif')

undisturbed <- ifel(disturbance_binaryAll > 0, NA, ForestMask)
writeRaster(undisturbed,
            "Data/czechia/undisturbed.tif",
            overwrite = TRUE)



disturbance_binary <- rast("Data/czechia/disturbance_binary_2011_2023.tif")


undisturbed <- rast("Data/czechia/undisturbed.tif")
# open the netcdf of biomass 2010

Biomass2010 <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc')

# clip Biomass2010 to the spatial extent of raster czechia (the extent of disturbance_binary)
# 1) Get czechia extent
czechia_extent <- ext(disturbance_binary)
czechia_crs <- crs(disturbance_binary)

# 2) Reproject the extent to Biomass2019 CRS
czechia_extent_transformed <- project(czechia_extent, 
                                      from = czechia_crs, 
                                      to = crs(Biomass2010))

# 3) Crop Biomass2010 using the transformed extent (fast!)
Biomass2010_cropped <- crop(Biomass2010, czechia_extent_transformed)
# 
# # # # 4) Reproject only the cropped portion to czechia CRS
# # # Biomass2010_final <- project(Biomass2010_cropped, disturbance_binary)
# 
# # # # Check result
# # # print(Biomass2010_final)
# # # plot(Biomass2010_final)
# 
# # # write Biomass2010_final raster and ignore in git
writeRaster(Biomass2010_cropped,'Data/Biomassczechia2010.tif',overwrite=TRUE)
Biomass2010_cropped <- rast('Data/Biomassczechia2010.tif')


# same with biomass 2020

Biomass2020 <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv6.0.nc')
# BiomassChange <- rast('Data/ESACCI-BIOMASS-L4-AGB-CHANGE-100m-2020-2010-fv6.0.nc')
# Crop Biomass2020 using the transformed extent (fast!)
Biomass2020_cropped <- crop(Biomass2020, czechia_extent_transformed)

# write Biomass2010_final raster and ignore in git
writeRaster(Biomass2020_cropped,'Data/Biomassczechia2020.tif')
Biomass2020_cropped <- rast('Data/Biomassczechia2020.tif')

# load EEA dataset

EEA_forest_type <- rast('Data/FTY_2015_100m_eu_03035_d02_Full/FTY_2015_100m_eu_03035_d02_full.tif')

EEA_forest_type_cropped <- crop(EEA_forest_type, czechia_extent)
rcl_matrix <- matrix(c(
  0, NA,    # all non-tree and non-forest areas -> NA (or use 0 if you want to keep it)
  1, 1,     # broadleaved forest -> 1 (Broadleaf)
  2, 2,     # coniferous forest -> 2 (Needleleaf)
  3, 3,     # mixed forest -> 3 (Mixed Forest)
  254, NA,  # unclassifiable -> NA
  255, NA   # outside area -> NA
), ncol = 2, byrow = TRUE)

# Reclassify
EEA_forest_type_cropped <- classify(EEA_forest_type_cropped, rcl_matrix)


writeRaster(EEA_forest_type_cropped,'Data/EEA_forest_type_croppedczechia.tif',overwrite=TRUE)
EEA_forest_type_cropped <- rast('Data/EEA_forest_type_croppedczechia.tif')




# General histograms for the entire study area
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# Check your rasters
print(Biomass2010_cropped)
print(disturbance_binary)
print(EEA_forest_type_cropped)


Biomass2010_reprojected <- project(Biomass2010_cropped[[1]],
                                   disturbance_binary,
                                   method = "bilinear")

# If EEA_forest_type is also in different CRS, reproject it
EEA_forest_type_reprojected <- project(EEA_forest_type_cropped,
                                       disturbance_binary,
                                       method = "near")  # Use "near" for categorical data

# Step 2: Resample to match exact grid alignment
# This ensures pixels align perfectly
Biomass2010_aligned <- resample(Biomass2010_reprojected,
                                disturbance_binary,
                                method = "bilinear")

EEA_forest_type_aligned <- resample(EEA_forest_type_reprojected,
                                    disturbance_binary,
                                    method = "near")

# Verify they now match
compareGeom(disturbance_binary, Biomass2010_aligned, EEA_forest_type_aligned)

writeRaster(Biomass2010_aligned,'Data/Biomassczechia2010_aligned.tif', overwrite=TRUE)
writeRaster(EEA_forest_type_aligned,'Data/EEA_forest_type_alignedczechia.tif', overwrite=TRUE)
Biomass2010_aligned <- rast('Data/Biomassczechia2010_aligned.tif')
EEA_forest_type_aligned <- rast('Data/EEA_forest_type_alignedczechia.tif')




# Step 3: Now mask biomass by disturbance
biomass_disturbed <- mask(Biomass2010_aligned, 
                          disturbance_binary, 
                          maskvalues = c(0, NA))


# Step 4: Stack and extract values
biomass_forest_stack <- c(biomass_disturbed[[1]], EEA_forest_type_aligned)
names(biomass_forest_stack) <- c("biomass", "forest_type")




# Get czechia extent from raster
czechia_extent <- ext(Biomass2010_aligned)

# Convert extent to polygon for clipping
czechia_bbox <- as.polygons(czechia_extent, crs = crs(Biomass2010_aligned))
czechia_bbox_sf <- st_as_sf(czechia_bbox)

# Ensure CRS match
if(st_crs(Hex) != st_crs(czechia_bbox_sf)) {
  cat("Reprojecting hexagons to match raster CRS...\n")
  Hex <- st_transform(Hex, st_crs(czechia_bbox_sf))
}

# Clip hexagons to czechia extent
cat("Clipping hexagons to czechia extent...\n")
cat("Original number of hexagons:", nrow(Hex), "\n")

Hex_czechia <- st_intersection(Hex, czechia_bbox_sf)


Hex_czechia <- Hex_czechia |> dplyr::filter(forest_count >0)


# Make sure hexagons have an ID
if(!"hex_ID" %in% names(Hex_czechia)) {
  Hex_czechia$hex_ID <- 1:nrow(Hex_czechia)
}



st_write(Hex_czechia |> select(-hex_id),'Data/Hexs/Hex_czechia.gpkg')





library(data.table)


# ============================================================================
# Function to extract data for a single hexagon
# ============================================================================
extract_hex_data <- function(hex_idx, hex_geom, raster_stack) {
  
  tryCatch({
    # Extract raster values within this hexagon
    hex_vals <- extract(raster_stack, 
                        hex_geom, 
                        ID = FALSE,
                        cells = FALSE,
                        xy = FALSE)
    
    # Convert to data.table
    dt <- data.table(hex_vals)
    
    # Filter valid data
    dt <- dt[!is.na(biomass) & !is.na(forest_type) & 
               forest_type %in% c(1, 2, 3) & biomass > 0]
    
    # Add hexagon ID
    if(nrow(dt) > 0) {
      dt[, hex_ID := hex_idx]
      return(dt)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    warning(sprintf("Error processing hexagon %d: %s", hex_idx, e$message))
    return(NULL)
  })
}

# ============================================================================
# OPTION 1: Sequential processing (safer, easier to debug)
# ============================================================================
cat("Processing", nrow(Hex_czechia), "hexagons sequentially...\n")

all_results <- vector("list", nrow(Hex_czechia))

for(i in 1:nrow(Hex_czechia)) {
  if(i %% 100 == 0) {
    cat(sprintf("Processing hexagon %d/%d (%.1f%%)\n", 
                i, nrow(Hex_czechia), 100*i/nrow(Hex_czechia)))
  }
  
  all_results[[i]] <- extract_hex_data(Hex_czechia$hex_ID[i], 
                                       Hex_czechia[i, ], 
                                       biomass_forest_stack)
}


cat("Combining results...\n")
biomass_hex_dt <- rbindlist(all_results[!sapply(all_results, is.null)])

# Add forest type labels
biomass_hex_dt[, forest_type_label := fcase(
  forest_type == 1, "Broadleaf",
  forest_type == 2, "Needleleaf",
  forest_type == 3, "Mixed Forest",
  default = NA_character_
)]

cat("Total valid pixels across all hexagons:", nrow(biomass_hex_dt), "\n")
cat("Number of hexagons with data:", length(unique(biomass_hex_dt$hex_id)), "\n")

fwrite(biomass_hex_dt, "Data/biomassczechia_by_hexagon_completeDisturbed.csv")

# 
# # 2. Summary statistics by hexagon and forest type
# hex_summary <- biomass_hex_dt[, .(
#   n_pixels = .N,
#   mean_biomass = mean(biomass),
#   median_biomass = median(biomass),
#   sd_biomass = sd(biomass),
#   min_biomass = min(biomass),
#   max_biomass = max(biomass)
#   # total_biomass = sum(biomass)
# ), by = .(hex_ID, forest_type_label)]
# 
# fwrite(hex_summary, "Data/biomassczechia_by_hexagon_summaryUndisturbed.csv")
# 
# 
# # 3. Summary by hexagon (all forest types combined)
# hex_summary_total <- biomass_hex_dt[, .(
#   n_pixels = .N,
#   mean_biomass = mean(biomass),
#   median_biomass = median(biomass),
#   # total_biomass = sum(biomass),
#   n_broadleaf = sum(forest_type == 1),
#   n_needleleaf = sum(forest_type == 2),
#   n_mixed = sum(forest_type == 3)
# ), by = hex_ID]
# 
# fwrite(hex_summary_total, "Data/biomassczechia_by_hexagon_totalUndisturbed.csv")
# 
# # 4. Merge summary back to spatial hexagons for mapping
# Hex_with_biomass <- merge(Hex_czechia, hex_summary_total, by = "hex_ID", all.x = TRUE)
# Hex_with_biomass <- Hex_with_biomass |> select(-hex_id)
# 
# st_write(Hex_with_biomass, "Data/gridczechia_forest_with_biomassUndisturbed.gpkg", delete_dsn = TRUE)
# 
# Hex_with_biomass2 <- merge(Hex_czechia, hex_summary, by = "hex_ID", all.x = TRUE)
# 
# Hex_with_biomass2 <- Hex_with_biomass2 |> select(-hex_id)
# st_write(Hex_with_biomass2, "Data/gridczechia_forest_with_biomassTypeUndisturbed.gpkg", delete_dsn = TRUE)
