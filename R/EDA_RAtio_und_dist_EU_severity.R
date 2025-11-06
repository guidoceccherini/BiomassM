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
library(data.table)

library(data.table)
library(tidyverse)
library(sf)
library(scales)

# Read data
biomass_hex_UND <- read_csv("Data/biomass_EU_by_hexagon_undisturbed_v2.csv")
biomass_hex_DIST <- read_csv("Data/biomass_EU_by_hexagon_disturbed_vNoHarvest2DistributionSeverity.csv")

setDT(biomass_hex_UND)
setDT(biomass_hex_DIST)

# Convert to tibbles
hex_summary_UND <- as_tibble(biomass_hex_UND)
hex_summary_DIST <- as_tibble(biomass_hex_DIST)

# Add columns to identify disturbed vs undisturbed
hex_summary_UND <- hex_summary_UND %>%
  mutate(disturbance_status = "Undisturbed")
hex_summary_DIST <- hex_summary_DIST %>%
  mutate(disturbance_status = "Disturbed")

# Drop NA in severity_bin
hex_summary_DIST <- hex_summary_DIST %>%
  filter(!is.na(severity_bin))

# Get all unique severity bins
severity_bins <- unique(hex_summary_DIST$severity_bin)
cat("Processing severity bins:", paste(severity_bins, collapse = ", "), "\n")

# Function to process each severity bin
process_severity_bin <- function(bin_value, hex_summary_UND, hex_summary_DIST) {
  
  # Filter for specific bin
  hex_summary_DIST_bin <- hex_summary_DIST %>%
    filter(severity_bin == bin_value) %>%
    dplyr::select(-severity_bin)
  
  # Combine both summaries
  hex_summary <- bind_rows(hex_summary_UND, hex_summary_DIST_bin)
  
  # Calculate ratio
  hex_summary_ratio <- hex_summary %>%
    filter(n_pixels >= 30) %>%
    dplyr::select(hex_ID, forest_type, mean_biomass, disturbance_status) %>%
    pivot_wider(names_from = disturbance_status, values_from = mean_biomass) %>%
    mutate(biomass_ratio_DIST_UND = Disturbed / Undisturbed)
  
  # Make hex_summary_ratio wider to get forest type in wide format
  hex_summary_ratio_wide <- hex_summary_ratio %>%
    dplyr::select(hex_ID, forest_type, biomass_ratio_DIST_UND) %>%
    pivot_wider(names_from = forest_type, values_from = biomass_ratio_DIST_UND, names_prefix = "ratio_")
  
  # Add severity bin column for identification
  hex_summary_ratio_wide <- hex_summary_ratio_wide %>%
    mutate(severity_bin = bin_value)
  
  return(hex_summary_ratio_wide)
}

# Process all severity bins
all_bins_data <- map_dfr(severity_bins, ~process_severity_bin(.x, hex_summary_UND, hex_summary_DIST))

# Open hexagons
Hex_EU <- st_read('Data/grid_forest.gpkg')
Hex_EU <- Hex_EU %>% dplyr::filter(forest_count > 0)

# Add hex ID if needed
if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}

# Join with hexagons (this will create one row per hex per severity bin)
Hex_EU_biomass_ratio_all <- Hex_EU %>%
  inner_join(all_bins_data, by = "hex_ID") %>%
  dplyr::select(-hex_ID)

# Transform to long format for plotting
Hex_EU_biomass_ratio_long <- Hex_EU_biomass_ratio_all %>%
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "forest_type",
    values_to = "ratio_value"
  ) %>%
  mutate(
    forest_type = gsub("ratio_", "Forest Type ", forest_type),
    severity_bin = factor(severity_bin, levels = c("1-5", "6", "7", "8", "9", "10-11"))
  )

# Create the faceted plot with severity bins as columns
p <- ggplot(Hex_EU_biomass_ratio_long) +
  geom_sf(aes(fill = ratio_value), color = NA) +
  scale_fill_gradient2(
    low = "#d73027",        # red for values below 1
    mid = "#ffffbf",        # yellow/white for 1
    high = "#1a9850",       # green for values above 1
    midpoint = 1,
    limits = c(0.9, 1.1),
    oob = squish,
    na.value = "grey90",
    name = "Biomass Ratio\n(Disturbed/Undisturbed)",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 20,
      barheight = 0.8
    )
  ) +
  facet_grid(
    forest_type ~ severity_bin,
    labeller = labeller(
      severity_bin = function(x) paste("Severity:", x)
    )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    title = "Biomass Ratios across Europe by Severity Bin and Forest Type",
    x = NULL,
    y = NULL
  )

print(p)

# Optional: Save the plot
ggsave("Figures/biomass_ratio_by_severity_all_bins.png", p,
       width = 16, height = 10, dpi = 300)

# Optional: Create separate plots for each forest type (cleaner visualization)
for(ft in unique(Hex_EU_biomass_ratio_long$forest_type)) {
  p_ft <- Hex_EU_biomass_ratio_long %>%
    filter(forest_type == ft) %>%
    ggplot() +
    geom_sf(aes(fill = ratio_value), color = NA) +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "#ffffbf",
      high = "#1a9850",
      midpoint = 1,
      limits = c(0.9, 1.1),
      oob = squish,
      na.value = "grey90",
      name = "Ratio",
      guide = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 0.8
      )
    ) +
    facet_wrap(~ severity_bin, ncol = 3) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    labs(
      title = paste("Biomass Ratios -", ft),
      x = NULL,
      y = NULL
    )
  
  print(p_ft)
  
  # Save individual plots
  filename <- paste0("Figures/biomass_ratio_", gsub(" ", "_", tolower(ft)), ".png")
  ggsave(filename, p_ft, width = 12, height = 8, dpi = 300)
}
