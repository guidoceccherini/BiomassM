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
# 
# # # Identify which layers correspond to 2015-2020
# # # Assuming layer 1 = 1985, layer 2 = 1986, etc.
# # # 2015 would be layer 31 (1985 + 30 = 2015)
# # # 2020 would be layer 36 (1985 + 35 = 2020)
# 
# # # Extract layers for 2015-2020 (layers 31-36)
# disturbances_2015_2020 <- Disturbances[[31:36]]
# 
# # # Check layer names to confirm
# names(disturbances_2015_2020)
# 
# # # Create binary raster: 1 if ANY disturbance in 2015-2020, 0 otherwise
# # # # Method 1: Using max() - if any layer has a value > 0
# disturbance_binary <- max(disturbances_2015_2020, na.rm = TRUE)
# 
# # # Convert to binary (1 if disturbed, 0 if not)
# disturbance_binary <- ifel(disturbance_binary > 0, 1, 0)
# 
# # # Check result
# # disturbance_binary
# plot(disturbance_binary, main = "Disturbances 2015-2020 (1 = disturbed)")
# 
# 
# # # Save the output
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

# Biomass2010_reprojected <- project(Biomass2010_cropped[[1]], 
#                                    disturbance_binary, 
#                                    method = "bilinear")
# 

# Biomass2020_reprojected <- project(Biomass2020_cropped[[1]],
#                                    disturbance_binary,
#                                    method = "bilinear")

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
# Biomass2020_aligned <- resample(Biomass2020_reprojected,
#                                 disturbance_binary,
#                                 method = "bilinear")


# EEA_forest_type_aligned <- resample(EEA_forest_type_reprojected, 
#                                     disturbance_binary, 
#                                     method = "near")
# 
# # Verify they now match
# compareGeom(disturbance_binary, Biomass2010_aligned, EEA_forest_type_aligned)
# 
# writeRaster(Biomass2010_aligned,'Data/Biomass2010_aligned.tif')
# writeRaster(Biomass2020_aligned,'Data/Biomass2020_aligned.tif')

# writeRaster(EEA_forest_type_aligned,'Data/EEA_forest_type_aligned.tif')
EEA_forest_type_aligned <- rast('Data/EEA_forest_type_aligned.tif')


Biomass2010_aligned <- rast('Data/Biomass2010_aligned.tif')
Biomass2020_aligned <- rast('Data/Biomass2020_aligned.tif')
DeltaBiomass <- Biomass2020_aligned - Biomass2010_aligned



# Filter only pixels with decrease in biomass > 10 Mg/ha

# Biomass2010_pos <- mask(Biomass2010_aligned, DeltaBiomass <  -10)
Biomass2010_pos <- ifel(DeltaBiomass < -10, Biomass2010_aligned, NA)


# Step 3: Now mask biomass by disturbance
biomass_disturbed <- mask(Biomass2010_pos, 
                          disturbance_binary, 
                          maskvalues = c(0, NA))

# Step 4: Stack and extract values
biomass_forest_stack <- c(biomass_disturbed[[1]], EEA_forest_type_aligned)
names(biomass_forest_stack) <- c("biomass", "forest_type")

library(terra)
library(data.table)
library(ggplot2)

# Create temporary file-based raster if not already
if(!sources(biomass_forest_stack)[1] != "") {
  temp_stack <- tempfile(fileext = ".tif")
  writeRaster(biomass_forest_stack, temp_stack, overwrite = TRUE)
  biomass_forest_stack <- rast(temp_stack)
}

# Extract all values efficiently using terra
# This reads from disk in chunks automatically
cat("Extracting values... This may take a few minutes.\n")

# Method: Use values() with chunks
process_in_chunks <- function(r, chunk_size = 5e7) {
  
  total_cells <- ncell(r)
  n_chunks <- ceiling(total_cells / chunk_size)
  
  result_list <- vector("list", n_chunks)
  
  for(i in 1:n_chunks) {
    start_row <- ceiling((i - 1) * chunk_size / ncol(r)) + 1
    end_row <- min(ceiling(i * chunk_size / ncol(r)), nrow(r))
    nrows_chunk <- end_row - start_row + 1
    
    cat(sprintf("Chunk %d/%d (rows %d-%d)\n", i, n_chunks, start_row, end_row))
    
    # Read chunk
    chunk_vals <- values(r, 
                         row = start_row, 
                         nrows = nrows_chunk,
                         dataframe = FALSE,
                         mat = TRUE)
    
    # Convert to data.table and filter
    dt <- data.table(
      biomass = chunk_vals[, 1],
      forest_type = chunk_vals[, 2]
    )
    
    # Filter in one step
    dt <- dt[!is.na(biomass) & !is.na(forest_type) & 
               forest_type %in% c(1, 2,3 ) & biomass > 0]
    
    result_list[[i]] <- dt
    
    rm(chunk_vals, dt)
    gc()
  }
  
  rbindlist(result_list)
}

# Process all data
biomass_dt <- process_in_chunks(biomass_forest_stack, chunk_size = 5e7)

# Add labels
# Add explicit forest type labels
# 2 = Needleleaf, 3 = Mixed Forest
biomass_dt[, forest_type_label := fcase(
  forest_type == 1,   "Broadleaf",
  forest_type == 2, "Needleleaf",
  forest_type == 3, "Mixed Forest",
  default = NA_character_
)]


# biomass_dt[, forest_type_label := fifelse(forest_type == 1, 
#                                           "Broadleaf", 
#                                           "Needleleaf")]

cat("Total valid pixels:", nrow(biomass_dt), "\n")

# Save to disk
fwrite(biomass_dt, "biomass_complete_data_deltaABG.csv")

# For plotting, you can work directly with data.table
# or convert to data.frame
biomass_df <- as.data.frame(biomass_dt)

# Create plots
p1 <- ggplot(biomass_df, aes(x = biomass, fill = forest_type_label)) +
  geom_density(alpha = 0.5) +
  labs(title = "Biomass Distribution in Disturbed Pixels",
       subtitle = paste("n =", format(nrow(biomass_df), big.mark = ",")),
       x = "Biomass (Mg/ha)", y = "Density", fill = "Forest Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p1)





library(terra)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)

# Load biogeoregions and convert to SpatVector
# Step 3: Rasterize biogeoregions
# BG_sv <- vect(BG)
# BG_raster <- rasterize(BG_sv, disturbance_binary, field = "code") # Use biogeoregion 'code'

gc()
gc()
gc()
gc()
gc()
gc()


# BG_raster <- rast('Data/BG_raster.tif')

# BG <- BG %>%
#   mutate(numeric_code = case_when(
#     code == "Alpine" ~ 1,
#     code == "Continental" ~ 2,
#     code == "Mediterranean" ~ 3,
#     code == "Pannonian" ~ 4,
#     TRUE ~ NA_integer_  # Any other regions get NA
#   ))
# 
# # Convert to SpatVector
# BG_sv <- vect(BG)
# 
# # Rasterize using the numeric_code field
# BG_numeric_raster <- rasterize(BG_sv,
#                                disturbance_binary,  # Use as template
#                                field = "numeric_code")
# 
# # Verify the values
# # cat("Unique values in numeric raster:\n")
# # print(unique(values(BG_numeric_raster)))
# 
# # Save the raster
# writeRaster(BG_numeric_raster, "Data/BG_numeric.tif", overwrite = TRUE)
# cat("Saved: BG_numeric.tif\n")
BG_numeric_raster <- rast('Data/BG_numeric.tif')



# Confirm raster values
# print(unique(values(BG_raster)))

# --- Step 4: Extract data in chunks with spatial join for biogeoregion ---



# Now stack biogeoregion with your other data
biomass_forest_bg_stack <- c(biomass_disturbed[[1]], 
                             EEA_forest_type_aligned, 
                             BG_numeric_raster)
names(biomass_forest_bg_stack) <- c("biomass", "forest_type", "biogeoregion")

# Process in chunks (using Method 4 from before)
process_in_chunks <- function(r, chunk_size = 5e7) {
  
  total_cells <- ncell(r)
  n_chunks <- ceiling(total_cells / chunk_size)
  
  result_list <- vector("list", n_chunks)
  
  for(i in 1:n_chunks) {
    start_row <- ceiling((i - 1) * chunk_size / ncol(r)) + 1
    end_row <- min(ceiling(i * chunk_size / ncol(r)), nrow(r))
    nrows_chunk <- end_row - start_row + 1
    
    cat(sprintf("Chunk %d/%d (rows %d-%d)\n", i, n_chunks, start_row, end_row))
    
    # Read chunk
    chunk_vals <- values(r, 
                         row = start_row, 
                         nrows = nrows_chunk,
                         dataframe = FALSE,
                         mat = TRUE)
    
    # Convert to data.table and filter
    dt <- data.table(
      biomass = chunk_vals[, 1],
      forest_type = chunk_vals[, 2],
      biogeoregion = chunk_vals[, 3]
    )
    
    # Filter in one step
    dt <- dt[!is.na(biomass) & !is.na(forest_type) & !is.na(biogeoregion) &
               forest_type %in% c(1, 2,3 ) & biomass > 0]
    
    result_list[[i]] <- dt
    
    rm(chunk_vals, dt)
    gc()
  }
  
  rbindlist(result_list)
}

# Process all data
cat("Extracting values... This may take a few minutes.\n")
biomass_dt <- process_in_chunks(biomass_forest_bg_stack, chunk_size = 5e7)

biomass_dt[, forest_type_label := fcase(
  forest_type == 1,   "Broadleaf",
  forest_type == 2, "Needleleaf",
  forest_type == 3, "Mixed Forest",
  default = NA_character_
)]


biomass_dt[, biogeoregion_label := fcase(
  biogeoregion == 1,   "Alpine",
  biogeoregion == 2, "Continental",
  biogeoregion == 3, "Mediterranean",
  biogeoregion == 4, "Pannonian",
  default = NA_character_
)]



# Save to disk for reference
fwrite(biomass_dt, "biomass_complete_data_with_bg_deltaABG.csv")

# Convert to data.frame for plotting
biomass_df <- as.data.frame(biomass_dt)

# -------- PLOTS WITH FACETS BY BIOGEOREGION --------

# Plot 1: Faceted density plots by biogeoregion and forest type
p1 <- ggplot(biomass_df, aes(x = biomass, fill = forest_type_label)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~biogeoregion_label, scales = "free") +
  labs(
    title = "Distribution of Biomass (2010) in Disturbed Pixels by Bio-geographical Region",
    subtitle = "Separated by Forest Type",
    x = "Above Ground Biomass (Mg/ha)",
    y = "Density",
    fill = "Forest Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "lightgray", size = 0.3),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p1)
ggsave("Figures/biomass_by_biogeoregion_density_AGBLOSS.png", p1, width = 16, height = 12, dpi = 300)

# Plot 2: Violin plots with facets by biogeoregion
p2 <- ggplot(biomass_df, aes(x = forest_type_label, y = biomass, 
                             fill = forest_type_label)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.15, alpha = 0.5, outlier.shape = NA) +
  facet_wrap(~biogeoregion_label, scales = "free_y") +
  labs(
    title = "Biomass Distribution in Disturbed Pixels by Bio-geographical Region",
    x = "Forest Type",
    y = "Above Ground Biomass (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p2)
ggsave("Figures/biomass_by_biogeoregion_violin.png", p2, width = 16, height = 12, dpi = 300)

# Plot 3: Boxplots comparing forest types within each biogeoregion
p3 <- ggplot(biomass_df, aes(x = forest_type_label, y = biomass, 
                             fill = forest_type_label)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 0.5) +
  facet_wrap(~biogeoregion_label, scales = "free_y") +
  labs(
    title = "Biomass (2010) in Disturbed Pixels by Bio-geographical Region",
    x = "Forest Type",
    y = "Above Ground Biomass (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p3)
ggsave("Figures/biomass_by_biogeoregion_boxplot_AGBLOSS.png", p3, width = 16, height = 12, dpi = 300)

# -------- SUMMARY STATISTICS BY BIOGEOREGION AND FOREST TYPE --------

# Detailed summary statistics
summary_stats <- biomass_dt[, .(
  n_pixels = .N,
  mean_biomass = mean(biomass, na.rm = TRUE),
  median_biomass = median(biomass, na.rm = TRUE),
  sd_biomass = sd(biomass, na.rm = TRUE),
  q25 = quantile(biomass, 0.25, na.rm = TRUE),
  q75 = quantile(biomass, 0.75, na.rm = TRUE),
  min_biomass = min(biomass, na.rm = TRUE),
  max_biomass = max(biomass, na.rm = TRUE)
), by = .(biogeoregion_label, forest_type_label)] %>%
  arrange(biogeoregion_label, forest_type_label)

# Format numbers
summary_stats <- summary_stats %>%
  mutate(across(where(is.numeric) & !n_pixels, ~round(., 2)))

print(summary_stats)

# Save summary to CSV
fwrite(summary_stats, "Data/biomass_summary_by_biogeoregion_forest_AGBLOSS.csv")

# -------- ALTERNATIVE: Side-by-side comparison within biogeoregions --------

# Create a summary plot comparing biogeoregions
summary_for_plot <- biomass_dt[, .(
  mean_biomass = mean(biomass, na.rm = TRUE),
  sd_biomass = sd(biomass, na.rm = TRUE),
  n = .N
), by = .(biogeoregion_label, forest_type_label)]

p4 <- ggplot(summary_for_plot, aes(x = biogeoregion_label, y = mean_biomass, 
                                   fill = forest_type_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, 
                    ymax = mean_biomass + sd_biomass),
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Mean Biomass (2010) in Disturbed Pixels",
    subtitle = "With standard deviation error bars",
    x = "Bio-geographical Region",
    y = "Mean Biomass (Mg/ha)",
    fill = "Forest Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p4)
ggsave("Figures/biomass_summary_by_biogeoregion_AGBLOSS.png", p4, width = 12, height = 6, dpi = 300)

# -------- Additional analysis: Pixel counts by biogeoregion --------

pixel_counts <- biomass_dt[, .(n_pixels = .N), 
                           by = .(biogeoregion_label, forest_type_label)]

p5 <- ggplot(pixel_counts, aes(x = biogeoregion_label, y = n_pixels, 
                               fill = forest_type_label)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Number of Disturbed Pixels (2015-2020) by Bio-geographical Region",
    x = "Bio-geographical Region",
    y = "Number of Pixels",
    fill = "Forest Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Broadleaf" = "#2E7D32", 
                               "Needleleaf" = "#1565C0", "Mixed Forest" = "orange"))

print(p5)
ggsave("Figures/pixel_counts_by_biogeoregion_AGBLOSS.png", p5, width = 12, height = 6, dpi = 300)

cat("\nAnalysis complete! Outputs saved:\n")
cat("- biomass_by_biogeoregion_density.png\n")
cat("- biomass_by_biogeoregion_violin.png\n")
cat("- biomass_by_biogeoregion_boxplot.png\n")
cat("- biomass_summary_by_biogeoregion.png\n")
cat("- pixel_counts_by_biogeoregion.png\n")
cat("- biomass_summary_by_biogeoregion_forest.csv\n")
cat("- biomass_complete_data_with_bg.csv\n")
