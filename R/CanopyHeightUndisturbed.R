library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# Read data
biomass_hex_UND <- read_csv("Data/biomass_EU_CHby_hexagon_undisturbed_vNoHarvestDistributions.csv")
# biomass_hex_DIST <- read_csv("Data/biomass_EU_CH_by_hexagon_disturbed_vNoHarvestSeverity.csv")

setDT(biomass_hex_UND)
# setDT(biomass_hex_DIST)

hex_summary_UND <- as_tibble(biomass_hex_UND) %>% mutate(disturbance_status = "Undisturbed")
# hex_summary_DIST <- as_tibble(biomass_hex_DIST) %>% 
#   mutate(disturbance_status = "Disturbed") %>%
#   filter(!is.na(severity_bin))

# Open hexagons
Hex_EU <- st_read('Data/grid_forest.gpkg')
Hex_EU <- Hex_EU %>% dplyr::filter(forest_count > 0)

if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}

# st_write(Hex_EU |> select(-hex_id), 'Data/grid_forest_withID.gpkg', delete_dsn = TRUE)


# Read biogeographic regions
BG <- st_read('Data/BiogeoRegions2016.shp')

# Use centroids for faster spatial join
cat("Computing hexagon centroids...\n")
Hex_EU_centroids <- st_centroid(Hex_EU)

cat("Simplifying biogeographic regions...\n")
BG_simple <- st_simplify(BG, preserveTopology = TRUE, dTolerance = 1000)

if(st_crs(Hex_EU_centroids) != st_crs(BG_simple)) {
  BG_simple <- st_transform(BG_simple, st_crs(Hex_EU_centroids))
}

cat("Performing spatial join...\n")
Hex_EU_with_BG <- st_join(
  Hex_EU_centroids %>% dplyr::select(hex_ID),
  BG_simple %>% dplyr::select(code),
  join = st_intersects,
  left = TRUE
)

Hex_EU_with_BG_df <- Hex_EU_with_BG %>% st_drop_geometry()

# Join with biomass ratio data
cat("Joining with biomass data...\n")
data_for_plot <- Hex_EU_with_BG_df %>%
  inner_join(hex_summary_UND, by = "hex_ID") %>%
  filter(!is.na(code), !is.na(mean_biomass)) %>%
  mutate(
       forest_type = factor(forest_type, labels = c("Forest Type 1", "Forest Type 2", "Forest Type 3"))
  )

cat("Biogeographic regions found:", paste(unique(data_for_plot$code), collapse = ", "), "\n")
cat("Total observations:", nrow(data_for_plot), "\n")

# ===== PLOT 1: Combined with improved labels =====
p_combined <- ggplot(data_for_plot, aes(x = mean_biomass/10, y = forest_type, fill = as.factor(forest_type))) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.5,
    alpha = 0.8
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5, alpha = 0.7) +
  scale_fill_viridis_d(
    option = "plasma",
    name = "Forest Type",
    # breaks = c(1, 2, 3, 4, 5, 6),
    # labels = c("Very Heavy", "Heavy", "Moderate", "Mild", "Very Mild", "None"),
    direction = -1  # Reverse so heavy disturbance is darker
  ) +
  # scale_x_continuous(limits = c(0, 200)) +
  facet_wrap(~ code, ncol = 4) +
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  labs(
    title = "Canopy Height by Disturbance Severity Across Biogeographic Regions",
    subtitle = "Distribution of Undisturbed Canopy Height (all forest types)",
    x = "Canopy Height Undisturbed",
    y = NULL
  )

print(p_combined)
ggsave("Figures/joy_division_CHUNDdist_by_region_combined.png", p_combined,
       width = 16, height = 10, dpi = 300)





# Remove severity levels that don't appear in any region
data_filtered_clean <- data_for_plot %>%
  group_by(code) %>%
  filter(n() > 50) %>%  # Keep severity classes with at least 50 total observations
  ungroup() 

# %>%
#   mutate(
#     severity_label = droplevels(severity_label)  # Remove unused factor levels
#   )


p_combined <- ggplot(data_filtered_clean, aes(x = mean_biomass/10, y = forest_type, fill = as.factor(forest_type))) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.5,
    alpha = 0.8
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5, alpha = 0.7) +
  scale_fill_viridis_d(
    # option = "plasma",
    name = "Forest Type",
    # breaks = c(1, 2, 3, 4, 5, 6),
    # labels = c("Very Heavy", "Heavy", "Moderate", "Mild", "Very Mild", "None"),
    direction = -1  # Reverse so heavy disturbance is darker
  ) +
  # scale_x_continuous(limits = c(0, 200)) +
  facet_wrap(~ code, ncol = 4) +
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  labs(
    title = "Canopy Height Across Biogeographic Regions",
    subtitle = "Distribution of Undisturbed Canopy Height (by forest types)",
    x = "Canopy Height Undisturbed",
    y = NULL
  )

print(p_combined)
