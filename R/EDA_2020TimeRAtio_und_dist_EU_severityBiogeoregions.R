library(tidyverse)
library(sf)
library(ggridges)
library(data.table)
library(viridis)

# Read data
biomass_hex_UND <- read_csv("Data/biomass_EU_2020Timeby_hexagon_undisturbed_vNoHarvestDistributions.csv")
biomass_hex_DIST <- read_csv("Data/biomass_EU_2020Time_NoHarvest2DistributionSeverity.csv")

setDT(biomass_hex_UND)
setDT(biomass_hex_DIST)

hex_summary_UND <- as_tibble(biomass_hex_UND) %>% mutate(disturbance_status = "Undisturbed")

hex_summary_DIST <- as_tibble(biomass_hex_DIST) %>% 
  mutate(disturbance_status = "Disturbed") %>%
  filter(!is.na(disturbed_bin))

disturbed_bins <- sort(unique(hex_summary_DIST$disturbed_bin))

process_disturbed_bin <- function(bin_value, hex_summary_UND, hex_summary_DIST) {
  hex_summary_DIST_bin <- hex_summary_DIST %>%
    filter(disturbed_bin == bin_value) %>%
    dplyr::select(-disturbed_bin)
  
  hex_summary <- bind_rows(hex_summary_UND, hex_summary_DIST_bin)
  
  hex_summary_ratio <- hex_summary %>%
    filter(n_pixels >= 10) %>%
    dplyr::select(hex_ID, forest_type, mean_biomass, disturbance_status) %>%
    pivot_wider(names_from = disturbance_status, values_from = mean_biomass) %>%
    mutate(biomass_2020 = Disturbed ) %>%  #/ Undisturbed
    mutate(disturbed_bin = bin_value) %>%
    dplyr::select(hex_ID, forest_type, biomass_2020, Undisturbed,disturbed_bin)
  
  return(hex_summary_ratio)
}

all_bins_data <- map_dfr(disturbed_bins, ~process_disturbed_bin(.x, hex_summary_UND, hex_summary_DIST))

all_bins_data <- all_bins_data |> filter(!is.na(biomass_2020))


# Calculate the percentage
percentage <- all_bins_data %>%
  summarise(
    count_less = sum(biomass_2020 < Undisturbed, na.rm = TRUE),
    total = n(),
    percentage = (count_less / total) * 100
  )

print(percentage)

percentage_time <- all_bins_data %>%
  group_by(disturbed_bin) %>%
  summarise(
    count_less = sum(biomass_2020 < Undisturbed, na.rm = TRUE),
    total = n(),
    percentage = (count_less / total) * 100
  )|>
  ungroup()

print(percentage_time)

# filter incorrect biomass data

all_bins_data <- all_bins_data |> filter(biomass_2020 < Undisturbed)






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
    time_label = case_when(
      disturbed_bin == 1 ~ "2008-10",
      disturbed_bin == 2 ~ "2011-13",
      disturbed_bin == 3 ~ "2014-16",
      disturbed_bin == 4 ~ "2017-19",
      # disturbed_bin == "9" ~ "Disturbed",
      # disturbed_bin == "10-11" ~ "Not Disturbed",
      TRUE ~ as.character(disturbed_bin)
    ),
    # Factor with logical order (most disturbed at top)
    time_label = factor(
      time_label,
      levels = c(
        "2008-10",
        "2011-13",
        "2014-16",
        "2017-19"
      )
    ),
    # Keep numeric version for color scale
    time_numeric = disturbed_bin,
    forest_type = factor(forest_type, labels = c("Forest Type 1", "Forest Type 2", "Forest Type 3"))
  )

cat("Biogeographic regions found:", paste(unique(data_for_plot$code), collapse = ", "), "\n")
cat("Total observations:", nrow(data_for_plot), "\n")

# ===== PLOT 1: Combined with improved labels =====
p_combined <- ggplot(data_for_plot, aes(x = biomass_2020, y = time_label, fill = time_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 7.5,
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
    title = "Biomass by Disturbance Severity Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass (all forest types)",
    x = "Biomass 2020 Disturbed",
    y = NULL
  )

print(p_combined)



p_combined2 <- ggplot(data_for_plot, aes(x = biomass_2020, y = time_label, fill = time_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 7.5,
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
  facet_grid(forest_type~ code) +
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
    title = "Biomass by Disturbance Severity Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass (all forest types)",
    x = "Biomass 2020 Disturbed",
    y = NULL
  )

print(p_combined2)




# Calculate sample sizes by region and severity

# Filter to keep only code-time combinations with >= 50 observations
data_for_plot_filtered <- data_for_plot %>%
  group_by(code, time_numeric,forest_type) %>%
  filter(n() >= 100) %>%
  ungroup()


p_combined3 <- ggplot(data_for_plot_filtered, aes(x = biomass_2020, y = time_label, fill = time_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 7.5,
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
  facet_grid(.~ code) +
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
    title = "Biomass by Disturbance Timing Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass (all forest types)",
    x = "Biomass 2020 Disturbed",
    y = NULL
  )

print(p_combined3)




p_combined4 <- ggplot(data_for_plot_filtered, aes(x = biomass_2020, y = time_label, fill = time_numeric)) +
  stat_density_ridges(
    aes(height = after_stat(density)),
    geom = "density_ridges_gradient",
    scale = 2.5,
    rel_min_height = 0.01,
    bandwidth = 7.5,
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
  facet_grid(forest_type~ code) +
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
    title = "Biomass by Disturbance Timing Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass (all forest types)",
    x = "Biomass 2020 Disturbed",
    y = NULL
  )

print(p_combined4)


# ggsave("Figures/joy_division_biomass2020_by_region_combined.png", p_combined, 
#        width = 16, height = 10, dpi = 300)



# compute Ratio
data_for_plot_filtered <- data_for_plot_filtered |>
  mutate(Ratio = (Undisturbed-biomass_2020) / Undisturbed)




p_combined5 <- ggplot(data_for_plot_filtered, aes(x = Ratio, y = time_label, fill = time_numeric)) +
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
    # breaks = c(1, 2, 3, 4, 5, 6),
    # labels = c("Very Heavy", "Heavy", "Moderate", "Mild", "Very Mild", "None"),
    direction = -1  # Reverse so heavy disturbance is darker
  ) +
  # scale_x_continuous(limits = c(0.5, 1.5)) +

facet_grid(.~ code) +
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
    title = "Ratio Biomass Disturbed/Undisturbed by Disturbance Timing Across Biogeographic Regions",
    subtitle = "Distribution of Disturbed biomass ratio (all forest types)",
    x = "Biomass Ratio Disturbed vs Undisturbed",
    y = NULL
  )

print(p_combined5)



