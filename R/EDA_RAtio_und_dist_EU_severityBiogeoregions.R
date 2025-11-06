library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# Read data
biomass_hex_UND <- read_csv("Data/biomass_EU_by_hexagon_undisturbed_v2.csv")
biomass_hex_DIST <- read_csv("Data/biomass_EU_by_hexagon_disturbed_vNoHarvest2DistributionSeverity.csv")

setDT(biomass_hex_UND)
setDT(biomass_hex_DIST)

hex_summary_UND <- as_tibble(biomass_hex_UND) %>% mutate(disturbance_status = "Undisturbed")
hex_summary_DIST <- as_tibble(biomass_hex_DIST) %>% 
  mutate(disturbance_status = "Disturbed") %>%
  filter(!is.na(severity_bin))

severity_bins <- unique(hex_summary_DIST$severity_bin)

process_severity_bin <- function(bin_value, hex_summary_UND, hex_summary_DIST) {
  hex_summary_DIST_bin <- hex_summary_DIST %>%
    filter(severity_bin == bin_value) %>%
    dplyr::select(-severity_bin)
  
  hex_summary <- bind_rows(hex_summary_UND, hex_summary_DIST_bin)
  
  hex_summary_ratio <- hex_summary %>%
    filter(n_pixels >= 30) %>%
    dplyr::select(hex_ID, forest_type, mean_biomass, disturbance_status) %>%
    pivot_wider(names_from = disturbance_status, values_from = mean_biomass) %>%
    mutate(biomass_ratio = Disturbed / Undisturbed) %>%
    mutate(severity_bin = bin_value) %>%
    dplyr::select(hex_ID, forest_type, biomass_ratio, severity_bin)
  
  return(hex_summary_ratio)
}

all_bins_data <- map_dfr(severity_bins, ~process_severity_bin(.x, hex_summary_UND, hex_summary_DIST))

# Open hexagons
Hex_EU <- st_read('Data/grid_forest.gpkg')
Hex_EU <- Hex_EU %>% dplyr::filter(forest_count > 0)

if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}

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
  inner_join(all_bins_data, by = "hex_ID") %>%
  filter(!is.na(code), !is.na(biomass_ratio)) %>%
  mutate(
    # Create readable severity labels
    severity_label = case_when(
      severity_bin == "1-5" ~ "Very Heavily Disturbed",
      severity_bin == "6" ~ "Heavily Disturbed",
      severity_bin == "7" ~ "Very Disturbed",
      severity_bin == "8" ~ "Mildly Disturbed",
      severity_bin == "9" ~ "Disturbed",
      severity_bin == "10-11" ~ "Not Disturbed",
      TRUE ~ as.character(severity_bin)
    ),
    # Factor with logical order (most disturbed at top)
    severity_label = factor(
      severity_label,
      levels = c(
        "Very Heavily Disturbed",
        "Heavily Disturbed",
        "Very Disturbed",
        "Mildly Disturbed",
        "Disturbed",
        "Not Disturbed"
      )
    ),
    # Keep numeric version for color scale
    severity_numeric = case_when(
      severity_bin == "1-5" ~ 1,
      severity_bin == "6" ~ 2,
      severity_bin == "7" ~ 3,
      severity_bin == "8" ~ 4,
      severity_bin == "9" ~ 5,
      severity_bin == "10-11" ~ 6
    ),
    forest_type = factor(forest_type, labels = c("Forest Type 1", "Forest Type 2", "Forest Type 3"))
  )

cat("Biogeographic regions found:", paste(unique(data_for_plot$code), collapse = ", "), "\n")
cat("Total observations:", nrow(data_for_plot), "\n")

# ===== PLOT 1: Combined with improved labels =====
p_combined <- ggplot(data_for_plot, aes(x = biomass_ratio, y = severity_label, fill = severity_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.02,
    alpha = 0.8
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5, alpha = 0.7) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Disturbance\nIntensity",
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = c("Very Heavy", "Heavy", "Moderate", "Mild", "Very Mild", "None"),
    direction = -1  # Reverse so heavy disturbance is darker
  ) +
  scale_x_continuous(limits = c(0.7, 1.3), breaks = seq(0.7, 1.3, 0.1)) +
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
    title = "Biomass Recovery by Disturbance Severity Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed/Undisturbed biomass ratios (all forest types)",
    x = "Biomass Ratio (Disturbed / Undisturbed)",
    y = NULL
  )

print(p_combined)
ggsave("Figures/joy_division_biomass_by_region_combined.png", p_combined, 
       width = 16, height = 10, dpi = 300)


# ===== PLOT 2: Classic Joy Division style with improved labels =====
p_classic <- ggplot(data_for_plot, aes(x = biomass_ratio, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.02
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", size = 0.3, alpha = 0.5) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 1,
    limits = c(0.7, 1.3),
    name = "Biomass Ratio"
  ) +
  scale_x_continuous(limits = c(0.7, 1.3), breaks = seq(0.7, 1.3, 0.1)) +
  facet_wrap(~ code, ncol = 4) +
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    axis.text.y = element_text(size = 9, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 9, color = "white"),
    axis.title = element_text(size = 11, face = "bold", color = "white"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "white"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "grey20"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  labs(
    title = "Forest Biomass Recovery Across European Biogeographic Regions",
    subtitle = "From heavily disturbed to undisturbed forests",
    x = "Biomass Ratio (Disturbed / Undisturbed)",
    y = NULL
  )

print(p_classic)
ggsave("Figures/joy_division_biomass_classic_style.png", p_classic, 
       width = 16, height = 10, dpi = 300, bg = "black")


# ===== PLOT 3: Reversed order (undisturbed at top) =====
data_for_plot_reversed <- data_for_plot %>%
  mutate(
    severity_label_reversed = factor(
      severity_label,
      levels = rev(levels(severity_label))
    )
  )

p_reversed <- ggplot(data_for_plot_reversed, aes(x = biomass_ratio, y = severity_label_reversed, fill = severity_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.02,
    alpha = 0.8
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5, alpha = 0.7) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Disturbance\nIntensity",
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = c("Very Heavy", "Heavy", "Moderate", "Mild", "Very Mild", "None"),
    direction = 1
  ) +
  scale_x_continuous(limits = c(0.7, 1.3), breaks = seq(0.7, 1.3, 0.1)) +
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
    title = "Biomass Recovery Gradient: Undisturbed to Heavily Disturbed",
    subtitle = "Distribution of Disturbed/Undisturbed biomass ratios",
    x = "Biomass Ratio (Disturbed / Undisturbed)",
    y = NULL
  )

print(p_reversed)
ggsave("Figures/joy_division_biomass_reversed.png", p_reversed, 
       width = 16, height = 10, dpi = 300)


# Summary statistics
summary_stats <- data_for_plot %>%
  group_by(code, severity_label) %>%
  summarise(
    n = n(),
    mean_ratio = mean(biomass_ratio, na.rm = TRUE),
    median_ratio = median(biomass_ratio, na.rm = TRUE),
    sd_ratio = sd(biomass_ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(code, severity_label)

print(summary_stats)
# write_csv(summary_stats, "biomass_ratio_summary_by_region.csv")
