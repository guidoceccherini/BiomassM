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



biomass_hex_UND <- read_csv("Data/biomass_EU_by_hexagon_undisturbed_v2.csv")

biomass_hex_DIST <- read_csv("Data/biomass_EU_by_hexagon_disturbed_v2.csv")

setDT(biomass_hex_UND)
setDT(biomass_hex_DIST)


# 
# # 2. Summary statistics by hexagon and forest type
# hex_summary_UND <- biomass_hex_UND[, .(
#   n_pixels = .N,
#   mean_biomass = mean(biomass),
#   median_biomass = median(biomass),
#   sd_biomass = sd(biomass),
#   min_biomass = min(biomass),
#   max_biomass = max(biomass)
#   # total_biomass = sum(biomass)
# ), by = .(hex_ID, forest_type_label)]
# 
# hex_summary_UND
# 
# 
# 
# hex_summary_DIST <- biomass_hex_DIST[, .(
#   n_pixels = .N,
#   mean_biomass = mean(biomass),
#   median_biomass = median(biomass),
#   sd_biomass = sd(biomass),
#   min_biomass = min(biomass),
#   max_biomass = max(biomass)
#   # total_biomass = sum(biomass)
# ), by = .(hex_ID, forest_type_label)]
# 
# hex_summary_DIST

# convert to tibbles
hex_summary_UND <- as_tibble(biomass_hex_UND)
hex_summary_DIST <- as_tibble(biomass_hex_DIST)

# add columns to identify disturbed vs undisturbed
hex_summary_UND <- hex_summary_UND %>%
  mutate(disturbance_status = "Undisturbed")
hex_summary_DIST <- hex_summary_DIST %>%
  mutate(disturbance_status = "Disturbed")

# combine both summaries
hex_summary <- bind_rows(hex_summary_UND, hex_summary_DIST)
hex_summary


# for each ID and forest type, calculate the ratio of mean biomass disturbed/undisturbed
# also keep the information of number of samples for disturbed and unisturbed to further filter for low number


hex_summary_ratio <- hex_summary %>%
  filter(n_pixels >= 30) %>%
  select(hex_ID, forest_type, mean_biomass, disturbance_status) %>%
  pivot_wider(names_from = disturbance_status, values_from = mean_biomass) %>%
  mutate(biomass_ratio_DIST_UND = Disturbed / Undisturbed)


# make hex_summary_ratio wider to get forest type in wide format
hex_summary_ratio_wide <- hex_summary_ratio %>%
  select(hex_ID, forest_type, biomass_ratio_DIST_UND) %>%
  pivot_wider(names_from = forest_type, values_from = biomass_ratio_DIST_UND, names_prefix = "ratio_")


# make hex_summary_ratio wider to get forest type in wide format
hex_summary_Undisturbed_wide <- hex_summary_ratio %>%
  select(hex_ID, forest_type, Undisturbed) %>%
  pivot_wider(names_from = forest_type, values_from = Undisturbed, names_prefix = "UND_")

hex_summary_Disturbed_wide <- hex_summary_ratio %>%
  select(hex_ID, forest_type, Disturbed) %>%
  pivot_wider(names_from = forest_type, values_from = Disturbed, names_prefix = "DIST_")



# open hexagons

Hex_EU <- st_read('Data/grid_forest.gpkg')

Hex_EU <- Hex_EU|> dplyr::filter(forest_count >0)

# Reproject if needed
# if(st_crs(Hex_EU) != st_crs(eu_stack)) {
#   Hex_EU <- st_transform(Hex_EU, st_crs(eu_stack))
# }

# Add hex ID
if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}



# join hex_summary_ratio with Hex_italy
Hex_EU_biomass_ratio <- Hex_EU %>% ###st_drop_geometry() %>%
  left_join(hex_summary_ratio_wide, by = c("hex_ID")) %>%
  left_join(hex_summary_Undisturbed_wide, by = c("hex_ID"))%>%
  left_join(hex_summary_Disturbed_wide, by = c("hex_ID"))

Hex_EU_biomass_ratio <- Hex_EU_biomass_ratio |> dplyr::select(-hex_ID)

# write Hex_italy_biomass_ratio to gpkg
st_write(Hex_EU_biomass_ratio, "Data/Hexs/Hex_EU_biomass_ratioUndisturbed.gpkg", delete_dsn = TRUE)

# fwrite(hex_summary, "Data/biomass_by_hexagon_summaryUndisturbed.csv")

library(ggplot2)
library(tidyr)
library(sf)
library(scales)

# Trasforma i dati in formato long
Hex_EU_biomass_ratio_long <- Hex_EU_biomass_ratio %>%
  pivot_longer(
    cols = c(ratio_1, ratio_2, ratio_3),
    names_to = "ratio_type",
    values_to = "ratio_value"
  )

# Crea il plot con colorscale orizzontale e limiti
ggplot(Hex_EU_biomass_ratio_long) +
  geom_sf(aes(fill = ratio_value), color = NA) +
  scale_fill_gradient2(
    low = "#d73027",        # rosso per valori sotto 1
    mid = "#ffffbf",        # giallo/bianco per 1
    high = "#1a9850",       # verde per valori sopra 1
    midpoint = 1,
    limits = c(0.9, 1.1),   # limiti della scala
    oob = squish,           # squish comprime i valori fuori range
    na.value = "grey90",
    name = "Ratio",
    guide = guide_colorbar(
      direction = "horizontal",  # colorbar orizzontale
      title.position = "top",    # titolo sopra la barra
      title.hjust = 0.5,         # centra il titolo
      barwidth = 15,             # larghezza della barra
      barheight = 0.8            # altezza della barra
    )
  ) +
  facet_wrap(~ ratio_type, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"  # posizione legenda in basso
  ) +
  labs(
    title = "Biomass Ratios across Europe",
    x = NULL,
    y = NULL
  )




library(ggplot2)
library(tidyr)
library(sf)

# Trasforma i dati UND in formato long
Hex_EU_biomass_UND_long <- Hex_EU_biomass_ratio %>%
  pivot_longer(
    cols = c(UND_1, UND_2, UND_3),
    names_to = "UND_type",
    values_to = "UND_value"
  )

# Crea il plot con scala Viridis
ggplot(Hex_EU_biomass_UND_long) +
  geom_sf(aes(fill = UND_value), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",      # opzioni: "viridis", "magma", "plasma", "inferno", "cividis"
    na.value = "grey90",
    name = "Biomass",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.8
    )
  ) +
  facet_wrap(~ UND_type, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Biomass (UND) across Europe",
    x = NULL,
    y = NULL
  )




# 
# library(ggplot2)
# library(tidyr)
# library(sf)


# now for degraded

# Trasforma i dati DEG in formato long
Hex_EU_biomass_DEG_long <- Hex_EU_biomass_ratio %>%
  pivot_longer(
    cols = c(DEG_1, DEG_2, DEG_3),
    names_to = "DEG_type",
    values_to = "DEG_value"
  )

# Crea il plot con scala Viridis
ggplot(Hex_EU_biomass_DEG_long) +
  geom_sf(aes(fill = DEG_value), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",      # opzioni: "viridis", "magma", "plasma", "inferno", "cividis"
    na.value = "grey90",
    name = "Biomass",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.8
    )
  ) +
  facet_wrap(~ DEG_type, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Biomass (DEG) across Europe",
    x = NULL,
    y = NULL
  )
