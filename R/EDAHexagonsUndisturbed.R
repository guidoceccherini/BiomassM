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

Disturbances <- rast('Data/italy/annual_disturbances_1985_2023_italy.tif')

# # Identify which layers correspond to 2015-2020
# # Assuming layer 1 = 1985, layer 2 = 1986, etc.
# # 2015 would be layer 31 (1985 + 30 = 2015)
# # 2020 would be layer 36 (1985 + 35 = 2020)

# # Extract layers for 2015-2020 (layers 31-36)
# disturbances_2015_2020 <- Disturbances[[31:36]]
# disturbances_2011_2023 <- Disturbances[[27:39]]

# # Check layer names to confirm
# names(disturbances_2015_2020)

# # Create binary raster: 1 if ANY disturbance in 2015-2020, 0 otherwise
# # # Method 1: Using max() - if any layer has a value > 0
# disturbance_binary <- max(disturbances_2015_2020, na.rm = TRUE)
# disturbance_binary11_23 <- max(disturbances_2011_2023, na.rm = TRUE)
# disturbance_binaryAll <- max(Disturbances, na.rm = TRUE)
# # Convert to binary (1 if disturbed, 0 if not)
# disturbance_binary <- ifel(disturbance_binary > 0, 1, 0)
# disturbance_binary11_23 <- ifel(disturbance_binary11_23 > 0, 1, 0)

# # Check result
# disturbance_binary
# plot(disturbance_binary, main = "Disturbances 2015-2020 (1 = disturbed)")
# plot(disturbance_binary11_23, main = "Disturbances 2011-2023 (1 = disturbed)")


# # Save the output
# writeRaster(disturbance_binary, 
#             "Data/italy/disturbance_binary_2015_2020.tif",
#             overwrite = TRUE)

# writeRaster(disturbance_binary11_23,
#             "Data/italy/disturbance_binary_2011_2023.tif",
#             overwrite = TRUE)


# undisturbed pixels
# ForestMask <- rast('Data/italy/forest_mask_italy.tif')
# 
# undisturbed <- ifel(disturbance_binaryAll > 0, NA, ForestMask)
# writeRaster(undisturbed,
#             "Data/italy/undisturbed.tif",
#             overwrite = TRUE)



disturbance_binary <- rast("Data/italy/disturbance_binary_2011_2023.tif")


undisturbed <- rast("Data/italy/undisturbed.tif")
# open the netcdf of biomass 2010

Biomass2010 <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc')

# clip Biomass2010 to the spatial extent of raster italy (the extent of disturbance_binary)
# 1) Get Italy extent
italy_extent <- ext(disturbance_binary)
italy_crs <- crs(disturbance_binary)

# 2) Reproject the extent to Biomass2019 CRS
italy_extent_transformed <- project(italy_extent, 
                                    from = italy_crs, 
                                    to = crs(Biomass2010))

# 3) Crop Biomass2010 using the transformed extent (fast!)
# Biomass2010_cropped <- crop(Biomass2010, italy_extent_transformed)
# 
# # # # 4) Reproject only the cropped portion to Italy CRS
# # # Biomass2010_final <- project(Biomass2010_cropped, disturbance_binary)
# 
# # # # Check result
# # # print(Biomass2010_final)
# # # plot(Biomass2010_final)
# 
# # # write Biomass2010_final raster and ignore in git
# writeRaster(Biomass2010_cropped,'Data/BiomassItaly2010.tif',overwrite=TRUE)
Biomass2010_cropped <- rast('Data/BiomassItaly2010.tif')


# same with biomass 2020

# Biomass2020 <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv6.0.nc')
# BiomassChange <- rast('Data/ESACCI-BIOMASS-L4-AGB-CHANGE-100m-2020-2010-fv6.0.nc')
# Crop Biomass2020 using the transformed extent (fast!)
# Biomass2020_cropped <- crop(Biomass2020, italy_extent_transformed)

# write Biomass2010_final raster and ignore in git
# writeRaster(Biomass2020_cropped,'Data/BiomassItaly2020.tif')
Biomass2020_cropped <- rast('Data/BiomassItaly2020.tif')

# load EEA dataset

# EEA_forest_type <- rast('Data/FTY_2015_100m_eu_03035_d02_Full/FTY_2015_100m_eu_03035_d02_full.tif')

# EEA_forest_type_cropped <- crop(EEA_forest_type, italy_extent)
# writeRaster(EEA_forest_type_cropped,'Data/EEA_forest_type_cropped.tif')
EEA_forest_type_cropped <- rast('Data/EEA_forest_type_cropped.tif')




# General histograms for the entire study area
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

# Check your rasters
print(Biomass2010_cropped)
print(disturbance_binary)
print(EEA_forest_type_cropped)

# Step 1: Reproject all rasters to a common CRS and resolution
# Option A: Reproject everything to match disturbance_binary (EPSG:3035)
# This is recommended as it's the native CRS for European data

# Biomass2010_reprojected <- project(Biomass2010_cropped[[1]], 
#                                    disturbance_binary, 
#                                    method = "bilinear")
# 
# # If EEA_forest_type is also in different CRS, reproject it
# EEA_forest_type_reprojected <- project(EEA_forest_type_cropped, 
#                                        disturbance_binary, 
#                                        method = "near")  # Use "near" for categorical data
# 
# # Step 2: Resample to match exact grid alignment
# # This ensures pixels align perfectly
# Biomass2010_aligned <- resample(Biomass2010_reprojected, 
#                                 disturbance_binary, 
#                                 method = "bilinear")
# 
# EEA_forest_type_aligned <- resample(EEA_forest_type_reprojected, 
#                                     disturbance_binary, 
#                                     method = "near")
# 
# # Verify they now match
# compareGeom(disturbance_binary, Biomass2010_aligned, EEA_forest_type_aligned)
# 
# writeRaster(Biomass2010_aligned,'Data/Biomass2010_aligned.tif')
# writeRaster(EEA_forest_type_aligned,'Data/EEA_forest_type_aligned.tif')
Biomass2010_aligned <- rast('Data/Biomass2010_aligned.tif')
EEA_forest_type_aligned <- rast('Data/EEA_forest_type_aligned.tif')





# Step 3: Now mask biomass by disturbance
biomass_undisturbed <- mask(Biomass2010_aligned, 
                          undisturbed, 
                          maskvalues = c(0, NA))

# Step 4: Stack and extract values
biomass_forest_stack <- c(biomass_undisturbed[[1]], EEA_forest_type_aligned)
names(biomass_forest_stack) <- c("biomass", "forest_type")




# Get Italy extent from raster
italy_extent <- ext(Biomass2010_aligned)

# Convert extent to polygon for clipping
italy_bbox <- as.polygons(italy_extent, crs = crs(Biomass2010_aligned))
italy_bbox_sf <- st_as_sf(italy_bbox)

# Ensure CRS match
if(st_crs(Hex) != st_crs(italy_bbox_sf)) {
  cat("Reprojecting hexagons to match raster CRS...\n")
  Hex <- st_transform(Hex, st_crs(italy_bbox_sf))
}

# Clip hexagons to Italy extent
cat("Clipping hexagons to Italy extent...\n")
cat("Original number of hexagons:", nrow(Hex), "\n")

Hex_italy <- st_intersection(Hex, italy_bbox_sf)


Hex_italy <- Hex_italy |> dplyr::filter(forest_count >0)


# Make sure hexagons have an ID
if(!"hex_ID" %in% names(Hex_italy)) {
  Hex_italy$hex_ID <- 1:nrow(Hex_italy)
}



# st_write(Hex_italy |> select(-hex_id),'Data/Hexs/Hex_italy.gpkg')





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
cat("Processing", nrow(Hex_italy), "hexagons sequentially...\n")

all_results <- vector("list", nrow(Hex_italy))

for(i in 1:nrow(Hex_italy)) {
  if(i %% 100 == 0) {
    cat(sprintf("Processing hexagon %d/%d (%.1f%%)\n", 
                i, nrow(Hex_italy), 100*i/nrow(Hex_italy)))
  }
  
  all_results[[i]] <- extract_hex_data(Hex_italy$hex_ID[i], 
                                       Hex_italy[i, ], 
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

fwrite(biomass_hex_dt, "Data/biomass_by_hexagon_completeUndisturbed.csv")


# 2. Summary statistics by hexagon and forest type
hex_summary <- biomass_hex_dt[, .(
  n_pixels = .N,
  mean_biomass = mean(biomass),
  median_biomass = median(biomass),
  sd_biomass = sd(biomass),
  min_biomass = min(biomass),
  max_biomass = max(biomass)
  # total_biomass = sum(biomass)
), by = .(hex_ID, forest_type_label)]

fwrite(hex_summary, "Data/biomass_by_hexagon_summaryUndisturbed.csv")


# 3. Summary by hexagon (all forest types combined)
hex_summary_total <- biomass_hex_dt[, .(
  n_pixels = .N,
  mean_biomass = mean(biomass),
  median_biomass = median(biomass),
  # total_biomass = sum(biomass),
  n_broadleaf = sum(forest_type == 1),
  n_needleleaf = sum(forest_type == 2),
  n_mixed = sum(forest_type == 3)
), by = hex_ID]

fwrite(hex_summary_total, "Data/biomass_by_hexagon_totalUndisturbed.csv")

# 4. Merge summary back to spatial hexagons for mapping
Hex_with_biomass <- merge(Hex_italy, hex_summary_total, by = "hex_ID", all.x = TRUE)
Hex_with_biomass <- Hex_with_biomass |> select(-hex_id)

st_write(Hex_with_biomass, "Data/grid_forest_with_biomassUndisturbed.gpkg", delete_dsn = TRUE)

Hex_with_biomass2 <- merge(Hex_italy, hex_summary, by = "hex_ID", all.x = TRUE)

Hex_with_biomass2 <- Hex_with_biomass2 |> select(-hex_id)
st_write(Hex_with_biomass2, "Data/grid_forest_with_biomassTypeUndisturbed.gpkg", delete_dsn = TRUE)

# ============================================================================
# Example visualizations
# ============================================================================

# Plot density for an example hexagon
example_hex_ID <- 25#10
biomass_df_hex <- as.data.frame(biomass_hex_dt[hex_ID == example_hex_ID])

if(nrow(biomass_df_hex) > 0) {
  p_hex <- ggplot(biomass_df_hex, aes(x = biomass, fill = forest_type_label)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Biomass Distribution - Hexagon", example_hex_ID),
         subtitle = paste("n =", format(nrow(biomass_df_hex), big.mark = ",")),
         x = "Biomass (Mg/ha)", y = "Density", fill = "Forest Type") +
    theme_minimal() +
    scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                                 "Needleleaf" = "#1565C0", 
                                 "Mixed Forest" = "orange"))
  
  print(p_hex)
  ggsave("Data/example_hexagon_biomass_densityUndisturbed.png", p_hex, width = 8, height = 5)
}

# Create map of total biomass by hexagon
library(tmap)

map_biomass <- tm_shape(Hex_with_biomass) +
  tm_fill("mean_biomass", 
          style = "quantile",
          palette = "YlOrRd",
          title = "Mean Biomass\n(Mg/ha)") +
  tm_borders(alpha = 0.3) +
  tm_layout(title = "Total Unisturbed Biomass by Hexagon",
            legend.outside = TRUE)

print(map_biomass)
tmap_save(map_biomass, "Data/biomass_by_hexagon_mapUndisturbed.png", width = 10, height = 8)


Hex_with_biomass2 <- Hex_with_biomass2 |> 
  # remove NA
  filter(!is.na(mean_biomass))
# Filter for only Broadleaf and Needleleaf
Hex_two_types <- Hex_with_biomass2 %>%
  filter(forest_type_label %in% c("Broadleaf", "Needleleaf", "Mixed Forest"))

# Create faceted map
map_biomass_two_facets <- tm_shape(Hex_two_types) +
  tm_fill("mean_biomass", 
          style = "quantile",
          palette = "YlOrRd",
          title = "Mean Biomass\n(Mg/ha)") +
  tm_borders(alpha = 0.3) +
  tm_facets(by = "forest_type_label", 
            nrow = 1,
            free.coords = FALSE) +
  tm_layout(title = "Total Unisturbed Biomass: Broadleaf vs Needleleaf",
            legend.outside = TRUE,
            panel.labels = c("Broadleaf", "Needleleaf", "Mixed Forest"))

print(map_biomass_two_facets)
tmap_save(map_biomass_two_facets, "Data/biomass_broadleaf_needleleaf_mapUndisturbed.png", 
          width = 12, height = 6)



# Optional: Create faceted plots by forest type
p_facet <- ggplot(as.data.frame(hex_summary), 
                  aes(x = mean_biomass, fill = forest_type_label)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  facet_wrap(~forest_type_label, scales = "free_y") +
  labs(title = "Distribution of Mean Biomass Across Hexagons",
       x = "Mean Biomass (Mg/ha)", y = "Number of Hexagons",
       fill = "Forest Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", 
                               "Mixed Forest" = "orange"))

print(p_facet)
ggsave("Data/hexagon_biomass_distribution_facetedUndisturbed.png", p_facet, width = 12, height = 6)
