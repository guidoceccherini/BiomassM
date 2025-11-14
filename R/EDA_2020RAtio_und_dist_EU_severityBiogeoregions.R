library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# Read data
biomass_hex_UND <- read_csv("Data/biomass_EU_2020by_hexagon_undisturbed_vNoHarvestDistributions.csv")
biomass_hex_DIST <- read_csv("Data/biomass_EU_2020_NoHarvest2DistributionSeverity.csv")

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
    filter(n_pixels >= 10) %>%
    dplyr::select(hex_ID, forest_type, mean_biomass, disturbance_status) %>%
    pivot_wider(names_from = disturbance_status, values_from = mean_biomass) %>%
    mutate(biomass_2020 = Disturbed ) %>%  #/ Undisturbed
    mutate(severity_bin = bin_value) %>%
    dplyr::select(hex_ID, forest_type, biomass_2020, severity_bin)
  
  return(hex_summary_ratio)
}

all_bins_data <- map_dfr(severity_bins, ~process_severity_bin(.x, hex_summary_UND, hex_summary_DIST))

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
  inner_join(all_bins_data, by = "hex_ID") %>%
  filter(!is.na(code), !is.na(biomass_2020)) %>%
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
p_combined <- ggplot(data_for_plot, aes(x = biomass_2020, y = severity_label, fill = severity_numeric)) +
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
  scale_x_continuous(limits = c(0, 200)) +
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
    title = "Biomass Loss by Disturbance Severity Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass (all forest types)",
    x = "Biomass 2020 Disturbed)",
    y = NULL
  )

print(p_combined)
# ggsave("Figures/joy_division_biomass2020_by_region_combined.png", p_combined, 
#        width = 16, height = 10, dpi = 300)


# ===== PLOT 2: Classic Joy Division style with improved labels =====
p_classic <- ggplot(data_for_plot, aes(x = biomass_2020, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 5
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", size = 0.3, alpha = 0.5) +
  scale_fill_viridis(
    # low = "#d73027",
    # mid = "#ffffbf",
    # high = "#1a9850",
    # midpoint = 1,
    # limits = c(0.7, 1.3),
    name = "Biomass Loss 2020"
  ) +
  scale_x_continuous(limits = c(0, 200)) +
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
    title = "Forest Biomass Loss Across European Biogeographic Regions",
    subtitle = "From heavily disturbed to undisturbed forests",
    x = "Biomass Loss 2020 (Disturbed)",
    y = NULL
  )

print(p_classic)
# ggsave("Figures/joy_division_biomass_classic_style.png", p_classic, 
#        width = 16, height = 10, dpi = 300, bg = "black")


# ===== PLOT 3: Reversed order (undisturbed at top) =====
data_for_plot_reversed <- data_for_plot %>%
  mutate(
    severity_label_reversed = factor(
      severity_label,
      levels = rev(levels(severity_label))
    )
  )



# Summary statistics
summary_stats <- data_for_plot %>%
  group_by(code, severity_label) %>%
  summarise(
    n = n(),
    mean_ratio = mean(biomass_2020, na.rm = TRUE),
    median_ratio = median(biomass_2020, na.rm = TRUE),
    sd_ratio = sd(biomass_2020, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(code, severity_label)

print(summary_stats)
# write_csv(summary_stats, "biomass_2020_summary_by_region.csv")









library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# [Previous data processing code remains the same until data_for_plot...]

# ... [all previous code until data_for_plot is created] ...

# ===== FILTER OUT SPARSE CATEGORIES =====

# Calculate sample sizes by region and severity
sample_sizes <- data_for_plot %>%
  group_by(code, severity_label) %>%
  summarise(n = n(), .groups = "drop")

# Identify regions with sufficient data (e.g., > 100 total observations)
regions_with_data <- sample_sizes %>%
  group_by(code) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  filter(total_n > 100) %>%  # Adjust threshold as needed
  pull(code)

cat("Regions with sufficient data:", paste(regions_with_data, collapse = ", "), "\n")

# Filter data to keep only regions with sufficient data
data_filtered <- data_for_plot %>%
  filter(code %in% regions_with_data)

# Also remove severity classes with very few observations per region
severity_counts <- data_filtered %>%
  group_by(code, severity_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 20)  # At least 20 observations per severity class

# Keep only severity classes that appear in the filtered data
data_filtered <- data_filtered %>%
  semi_join(severity_counts, by = c("code", "severity_label"))

cat("Total observations after filtering:", nrow(data_filtered), "\n")

# ===== PLOT WITH IMPROVED LAYOUT =====

p_classic <- ggplot(data_filtered, aes(x = biomass_2020, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 10
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", size = 0.5, alpha = 0.7) +
  scale_fill_viridis(
    # low = "#d73027",
    # mid = "#ffffbf",
    # high = "#1a9850",
    # midpoint = 1,
    limits = c(0, 200),
    name = "Biomass Loss [t/ha]",
    guide = guide_colorbar(
      barwidth = 25,        # ENLARGED horizontal bar
      barheight = 1.2,      # TALLER bar
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "white",
      ticks.colour = "white"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 200), 
    # breaks = seq(0.7, 1.3, 0.1),
    # expand = c(0, 0)
  ) +
  facet_wrap(~ code, ncol = 3, scales = "free_y") +  # Adjust ncol based on remaining regions
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0.5, "cm"),
    legend.margin = margin(t = 10, b = 5),
    strip.text = element_text(size = 14, face = "bold", color = "white"),
    axis.text.y = element_text(size = 10, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 10, color = "white"),
    axis.title = element_text(size = 12, face = "bold", color = "white"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "white", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "white", margin = margin(b = 15)),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "grey20"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 10),
    legend.title = element_text(color = "white", size = 11, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Forest Biomass Loss European Biogeographic Regions",
    subtitle = "From heavily disturbed to undisturbed forests",
    x = "Biomass Loss 2020 (Disturbed)",
    y = NULL
  )

print(p_classic)
ggsave("Figures/joy_division_biomassLoss2020_filtered.png", p_classic, 
       width = 14, height = 10, dpi = 300, bg = "black")


# ===== ALTERNATIVE: AUTOMATIC FILTERING OF EMPTY SEVERITY CLASSES =====

# Remove severity levels that don't appear in any region
data_filtered_clean <- data_filtered %>%
  group_by(severity_label) %>%
  filter(n() > 50) %>%  # Keep severity classes with at least 50 total observations
  ungroup() %>%
  mutate(
    severity_label = droplevels(severity_label)  # Remove unused factor levels
  )

p_clean <- ggplot(data_filtered_clean, aes(x = biomass_2020, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.8,
    rel_min_height = 0.01,
    bandwidth = 10
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", size = 0.5, alpha = 0.7) +
  scale_fill_viridis(
    # low = "#d73027",
    # mid = "#ffffbf",
    # high = "#1a9850",
    # midpoint = 1,
    limits = c(0, 200),
    name = "Biomass Loss 2020 (Disturbed) [t/ha]",
    guide = guide_colorbar(
      barwidth = 30,        # VERY WIDE horizontal bar
      barheight = 1.5,      
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "white",
      frame.linewidth = 0.5,
      ticks.colour = "white",
      ticks.linewidth = 0.5
    )
  ) +
  scale_x_continuous(
    limits = c(0., 200), 
    # breaks = seq(0.7, 1.3, 0.1),
    # expand = c(0.01, 0.01)
  ) +
  facet_wrap(~ code, ncol = 3) +
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(1, "cm"),
    legend.margin = margin(t = 15, b = 10),
    strip.text = element_text(size = 15, face = "bold", color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "white", face = "bold"),
    axis.text.x = element_text(size = 11, color = "white"),
    axis.title = element_text(size = 13, face = "bold", color = "white"),
    axis.title.x = element_text(margin = margin(t = 15)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "white", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "white", margin = margin(b = 20)),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(20, 20, 20, 20),
    strip.background = element_rect(fill = "grey20"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 11),
    legend.title = element_text(color = "white", size = 12, face = "bold"),
    panel.spacing = unit(1.5, "lines")
  ) +
  labs(
    title = "Forest Biomass Loss Across European Biogeographic Regions",
    subtitle = "Distribution of biomass loss by disturbance severity",
    x = "Biomass Loss 2020",
    y = NULL
  )

print(p_clean)
ggsave("Figures/joy_division_biomass2020_clean.png", p_clean, 
       width = 15, height = 11, dpi = 300, bg = "black")





# now compute Ratio


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
    filter(n_pixels >= 10) %>%
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
    bandwidth = .05,
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
ggsave("Figures/joy_division_biomass2020_by_region_combined.png", p_combined, 
       width = 16, height = 10, dpi = 300)



# ===== PLOT 2: Classic Joy Division style with improved labels =====
p_classic <- ggplot(data_for_plot, aes(x = biomass_ratio, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.04
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
ggsave("Figures/joy_division_biomassratio2020_classic_style.png", p_classic, 
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
    bandwidth = 0.04,
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
ggsave("Figures/joy_division_biomass_reversedratio2020.png", p_reversed, 
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









library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# [Previous data processing code remains the same until data_for_plot...]

# ... [all previous code until data_for_plot is created] ...

# ===== FILTER OUT SPARSE CATEGORIES =====

# Calculate sample sizes by region and severity
sample_sizes <- data_for_plot %>%
  group_by(code, severity_label) %>%
  summarise(n = n(), .groups = "drop")

# Identify regions with sufficient data (e.g., > 100 total observations)
regions_with_data <- sample_sizes %>%
  group_by(code) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  filter(total_n > 100) %>%  # Adjust threshold as needed
  pull(code)

cat("Regions with sufficient data:", paste(regions_with_data, collapse = ", "), "\n")

# Filter data to keep only regions with sufficient data
data_filtered <- data_for_plot %>%
  filter(code %in% regions_with_data)

# Also remove severity classes with very few observations per region
severity_counts <- data_filtered %>%
  group_by(code, severity_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 20)  # At least 20 observations per severity class

# Keep only severity classes that appear in the filtered data
data_filtered <- data_filtered %>%
  semi_join(severity_counts, by = c("code", "severity_label"))

cat("Total observations after filtering:", nrow(data_filtered), "\n")

# ===== PLOT WITH IMPROVED LAYOUT =====

p_classic <- ggplot(data_filtered, aes(x = biomass_ratio, y = severity_label, fill = after_stat(x))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 0.04
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "white", size = 0.5, alpha = 0.7) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 1,
    limits = c(0.7, 1.3),
    name = "Biomass Ratio",
    guide = guide_colorbar(
      barwidth = 25,        # ENLARGED horizontal bar
      barheight = 1.2,      # TALLER bar
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "white",
      ticks.colour = "white"
    )
  ) +
  scale_x_continuous(
    limits = c(0.7, 1.3), 
    breaks = seq(0.7, 1.3, 0.1),
    expand = c(0, 0)
  ) +
  facet_wrap(~ code, ncol = 3, scales = "free_y") +  # Adjust ncol based on remaining regions
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0.5, "cm"),
    legend.margin = margin(t = 10, b = 5),
    strip.text = element_text(size = 14, face = "bold", color = "white"),
    axis.text.y = element_text(size = 10, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 10, color = "white"),
    axis.title = element_text(size = 12, face = "bold", color = "white"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "white", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "white", margin = margin(b = 15)),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "grey20"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 10),
    legend.title = element_text(color = "white", size = 11, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Forest Biomass Recovery Across European Biogeographic Regions",
    subtitle = "From heavily disturbed to undisturbed forests",
    x = "Biomass Ratio (Disturbed / Undisturbed)",
    y = NULL
  )

print(p_classic)
ggsave("Figures/joy_division_biomassratio2020_filtered.png", p_classic, 
       width = 14, height = 10, dpi = 300, bg = "black")


# ===== ALTERNATIVE: AUTOMATIC FILTERING OF EMPTY SEVERITY CLASSES =====
