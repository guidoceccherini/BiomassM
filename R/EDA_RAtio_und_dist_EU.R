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
if(st_crs(Hex_EU) != st_crs(eu_stack)) {
  Hex_EU <- st_transform(Hex_EU, st_crs(eu_stack))
}

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