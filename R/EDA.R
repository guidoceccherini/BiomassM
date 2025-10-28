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

# Disturbances <- rast('Data/italy/annual_disturbances_1985_2023_italy.tif')

# # Identify which layers correspond to 2015-2020
# # Assuming layer 1 = 1985, layer 2 = 1986, etc.
# # 2015 would be layer 31 (1985 + 30 = 2015)
# # 2020 would be layer 36 (1985 + 35 = 2020)

# # Extract layers for 2015-2020 (layers 31-36)
# disturbances_2015_2020 <- Disturbances[[31:36]]

# # Check layer names to confirm
# names(disturbances_2015_2020)

# # Create binary raster: 1 if ANY disturbance in 2015-2020, 0 otherwise
# # # Method 1: Using max() - if any layer has a value > 0
# disturbance_binary <- max(disturbances_2015_2020, na.rm = TRUE)

# # Convert to binary (1 if disturbed, 0 if not)
# disturbance_binary <- ifel(disturbance_binary > 0, 1, 0)

# # Check result
# disturbance_binary
# plot(disturbance_binary, main = "Disturbances 2015-2020 (1 = disturbed)")


# # Save the output
# writeRaster(disturbance_binary, 
#             "Data/italy/disturbance_binary_2015_2020.tif",
#             overwrite = TRUE)


disturbance_binary <- rast("Data/italy/disturbance_binary_2015_2020.tif")

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

# Step 3: Now mask biomass by disturbance
biomass_disturbed <- mask(Biomass2010_aligned, 
                          disturbance_binary, 
                          maskvalues = c(0, NA))

# Step 4: Stack and extract values
biomass_forest_stack <- c(biomass_disturbed[[1]], EEA_forest_type_aligned)
names(biomass_forest_stack) <- c("biomass", "forest_type")

# Convert to data frame
biomass_df <- as.data.frame(biomass_forest_stack, na.rm = TRUE)

# Step 5: Prepare data for plotting
biomass_df <- biomass_df %>%
  filter(forest_type %in% c(1, 2, 3), 
         !is.na(biomass), 
         biomass > 0) %>%
  mutate(forest_type_label = case_when(
    forest_type == 1 ~ "Broadleaf",
    forest_type == 2 ~ "Needleleaf",
    forest_type == 3 ~ "Mixed",
    
  ))

# Step 6: Create plots
# Density plot with overlay
p1 <- ggplot(biomass_df, aes(x = biomass, fill = forest_type_label)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Biomass (2010) in Disturbed Pixels (2015-2020)",
    x = "Above Ground Biomass (Mg/ha)",
    y = "Density",
    fill = "Forest Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0",
                               "Mixed" = "orange"))

# Faceted density plots
p2 <- ggplot(biomass_df, aes(x = biomass, fill = forest_type_label)) +
  geom_density(alpha = 0.7, color = "black", linewidth = 0.3) +
  facet_wrap(~forest_type_label, ncol = 1, scales = "free_y") +
  labs(
    title = "Biomass Distribution in Disturbed Pixels by Forest Type",
    x = "Above Ground Biomass (Mg/ha)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0",
                               "Mixed" = "orange"))

# Violin plot for comparison
p3 <- ggplot(biomass_df, aes(x = forest_type_label, y = biomass, 
                             fill = forest_type_label)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2, alpha = 0.5, outlier.shape = NA) +
  labs(
    title = "Biomass Distribution Comparison",
    x = "Forest Type",
    y = "Above Ground Biomass (Mg/ha)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0",
                               "Mixed" = "orange"))

# Display plots
print(p1)
print(p2)
print(p3)

# Step 7: Summary statistics
summary_stats <- biomass_df %>%
  group_by(forest_type_label) %>%
  summarise(
    n_pixels = n(),
    mean_biomass = mean(biomass, na.rm = TRUE),
    median_biomass = median(biomass, na.rm = TRUE),
    sd_biomass = sd(biomass, na.rm = TRUE),
    q25 = quantile(biomass, 0.25, na.rm = TRUE),
    q75 = quantile(biomass, 0.75, na.rm = TRUE),
    min_biomass = min(biomass, na.rm = TRUE),
    max_biomass = max(biomass, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(summary_stats)

# Save results
write.csv(summary_stats, "biomass_summary_by_forest_type.csv", row.names = FALSE)
ggsave("biomass_distribution_overlay.png", p1, width = 10, height = 6, dpi = 300)
ggsave("biomass_distribution_faceted.png", p2, width = 10, height = 8, dpi = 300)
ggsave("biomass_distribution_violin.png", p3, width = 8, height = 6, dpi = 300)






