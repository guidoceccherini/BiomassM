library(tidyverse)
library(terra)
library(sf)
library(data.table)


base_dir <- "/home/cecchgu/Downloads/13333034/"

# Get all country folders
country_folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)

cat("Found", length(country_folders), "country folders\n")

# Initialize lists to store file paths
disturbance_binary_files <- list()
undisturbed_files <- list()

# Process each country folder
for(folder in country_folders) {
  
  country_name <- basename(folder)
  cat("\nProcessing:", country_name, "\n")
  
  # Find the disturbances and forest mask files
   # Define output paths
  output_dist <- file.path(folder, "disturbance_binary_2011_2023.tif")
  output_undist <- file.path(folder, "undisturbed.tif")
  
 
  
  # Store file paths for VRT creation
  disturbance_binary_files <- c(disturbance_binary_files, output_dist)
  undisturbed_files <- c(undisturbed_files, output_undist)
  
  cat(sprintf("  ✓ Saved: %s\n", basename(output_dist)))
  cat(sprintf("  ✓ Saved: %s\n", basename(output_undist)))
  
  # Clean up memory
 
}

cat("\n=== Processing complete ===\n")
cat("Processed countries:", length(disturbance_binary_files), "\n")



# Create VRTs for EU-wide datasets
cat("\nCreating EU-wide virtual rasters...\n")

# VRT for disturbance binary 2011-2023
disturbance_eu_vrt <- vrt(unlist(disturbance_binary_files), 
                          "Data/disturbance_binary_2011_2023_EU.vrt", 
                          overwrite = TRUE)

# VRT for undisturbed forests
undisturbed_eu_vrt <- vrt(unlist(undisturbed_files), 
                          "Data/undisturbed_EU.vrt", 
                          overwrite = TRUE)

cat("✓ Created: disturbance_binary_2011_2023_EU.vrt\n")
cat("✓ Created: undisturbed_EU.vrt\n")

# Check VRTs
cat("\nDisturbance VRT info:\n")
print(disturbance_eu_vrt)

cat("\nUndisturbed VRT info:\n")
print(undisturbed_eu_vrt)



library(sf)
library(data.table)

# Load 
biomass_vrt <- rast('Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc')
forest_type_vrt <- rast('Data/FTY_2015_100m_eu_03035_d02_Full/FTY_2015_100m_eu_03035_d02_full.tif')

biomass_vrt <- biomass_vrt[[1]]

EU_extent <- ext(disturbance_eu_vrt)
EU_crs <- crs(disturbance_eu_vrt)


# EEA_forest_type_cropped <- crop(forest_type_vrt, EU_extent)
# rcl_matrix <- matrix(c(
#   0, NA,    # all non-tree and non-forest areas -> NA (or use 0 if you want to keep it)
#   1, 1,     # broadleaved forest -> 1 (Broadleaf)
#   2, 2,     # coniferous forest -> 2 (Needleleaf)
#   3, 3,     # mixed forest -> 3 (Mixed Forest)
#   254, NA,  # unclassifiable -> NA
#   255, NA   # outside area -> NA
# ), ncol = 2, byrow = TRUE)
# 
# EEA_forest_type_cropped <- classify(EEA_forest_type_cropped, rcl_matrix)
# 
# terra::writeRaster(EEA_forest_type_cropped,
#             "Data/EEA_forest_type_cropped_EU.tif",
#             overwrite = TRUE,
#             gdal = c("COMPRESS=LZW", "TILED=YES"))

EEA_forest_type_cropped <- rast("Data/EEA_forest_type_cropped_EU.tif")

# check extents


# Per biomass con ricampionamento bilinear
# Estrai i parametri come valori numerici singoli
ref_ext <- ext(disturbance_eu_vrt)
ref_res <- res(disturbance_eu_vrt)
ref_crs <- crs(disturbance_eu_vrt, proj=TRUE)
# sf::gdal_utils(
#   util = "warp",
#   source = 'NETCDF:"Data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv6.0.nc":agb',  # sostituisci 'agb' con il nome corretto
#   destination = "Data/Biomass2010_alignedEU.tif",
#   options = c(
#     "-t_srs", crs(disturbance_eu_vrt, proj=TRUE),
#     "-te", sprintf("%.6f", ref_ext[1]), sprintf("%.6f", ref_ext[3]),
#     sprintf("%.6f", ref_ext[2]), sprintf("%.6f", ref_ext[4]),
#     "-tr", sprintf("%.6f", ref_res[1]), sprintf("%.6f", ref_res[2]),
#     "-r", "bilinear",
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-co", "COMPRESS=LZW",
#     "-co", "TILED=YES",
#     "-co", "BIGTIFF=YES"
#   )
# )
Biomass2010_aligned <- rast("Data/Biomass2010_alignedEU.tif")


ref_ext <- as.vector(ext(disturbance_eu_vrt))
ref_res <- res(disturbance_eu_vrt)

# sf::gdal_utils(
#   util = "warp",
#   source = "Data/EEA_forest_type_cropped_EU.tif",  # modifica il path se necessario
#   destination = "Data/EEA_forest_type_alignedEU.tif",
#   options = c(
#     "-t_srs", crs(disturbance_eu_vrt, proj=TRUE),
#     "-te", sprintf("%.6f", ref_ext[1]), sprintf("%.6f", ref_ext[3]),
#     sprintf("%.6f", ref_ext[2]), sprintf("%.6f", ref_ext[4]),
#     "-tr", sprintf("%.6f", ref_res[1]), sprintf("%.6f", ref_res[2]),
#     "-r", "near",  # nearest neighbor per dati categorici
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-co", "COMPRESS=LZW",
#     "-co", "TILED=YES",
#     "-co", "BIGTIFF=YES"
#   )
# )

# Ricarica i risultati in terra
EEA_forest_type_aligned <- rast("Data/EEA_forest_type_alignedEU.tif")
# 
# Biomass2010_reprojected <- project(biomass_vrt[[1]],
#                                    disturbance_eu_vrt,
#                                    method = "bilinear")

# If EEA_forest_type is also in different CRS, reproject it
# EEA_forest_type_reprojected <- project(EEA_forest_type_cropped,
#                                        disturbance_eu_vrt,
#                                        method = "near")  # Use "near" for categorical data

# Step 2: Resample to match exact grid alignment
# This ensures pixels align perfectly
# Biomass2010_aligned <- resample(Biomass2010_reprojected,
#                                 disturbance_binary,
#                                 method = "bilinear")
# 
# EEA_forest_type_aligned <- resample(EEA_forest_type_reprojected,
#                                     disturbance_binary,
#                                     method = "near")

# Verify they now match
compareGeom(disturbance_eu_vrt, Biomass2010_aligned, EEA_forest_type_aligned)






# Stack all together
eu_stack <- c(Biomass2010_aligned, undisturbed_eu_vrt, EEA_forest_type_aligned)
names(eu_stack) <- c("biomass", "undisturbed", "forest_type")

# Load EU hexagon grid
Hex_EU <- st_read("Data/grid_forest.gpkg")

Hex_EU <- Hex_EU|> dplyr::filter(forest_count >0)

# Reproject if needed
if(st_crs(Hex_EU) != st_crs(eu_stack)) {
  Hex_EU <- st_transform(Hex_EU, st_crs(eu_stack))
}

# Add hex ID
if(!"hex_ID" %in% names(Hex_EU)) {
  Hex_EU$hex_ID <- 1:nrow(Hex_EU)
}

# Extract function
extract_hex_data <- function(hex_idx, hex_geom, raster_stack) {
  tryCatch({
    hex_vals <- extract(raster_stack, hex_geom, ID = FALSE)
    dt <- data.table(hex_vals)
    
    # Filter: only undisturbed pixels with valid forest type and biomass
    dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
               !is.na(biomass) & biomass > 0 &
               !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
    
    if(nrow(dt) > 0) {
      dt[, hex_ID := hex_idx]
      return(dt)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Process all hexagons
cat("Processing", nrow(Hex_EU), "hexagons across EU...\n")

all_results <- vector("list", nrow(Hex_EU))

for(i in 1:nrow(Hex_EU)) {
  if(i %% 1000 == 0) {
    cat(sprintf("[%s] Hexagon %d/%d (%.1f%%)\n", 
                Sys.time(), i, nrow(Hex_EU), 100*i/nrow(Hex_EU)))
    gc()
  }
  
  all_results[[i]] <- extract_hex_data(Hex_EU$hex_ID[i], 
                                       Hex_EU[i, ], 
                                       eu_stack)
}

# 
# 
# # alternatives1
# library(future.apply)
# library(data.table)
# 
# # Setup parallelizzazione
# plan(multisession, workers = availableCores() - 1)  # lascia 1 core libero
# 
# # Dividi gli hexagoni in gruppi (uno per core)
# n_cores <- availableCores() - 1
# hex_groups <- split(1:nrow(Hex_EU), cut(1:nrow(Hex_EU), n_cores, labels = FALSE))
# 
# # Funzione per processare un gruppo di hexagoni
# process_hex_group <- function(hex_indices, hex_sf, raster_stack) {
#   library(terra)
#   library(data.table)
#   
#   group_results <- vector("list", length(hex_indices))
#   
#   for(i in seq_along(hex_indices)) {
#     idx <- hex_indices[i]
#     
#     tryCatch({
#       hex_vals <- extract(raster_stack, hex_sf[idx, ], ID = FALSE)
#       dt <- data.table(hex_vals)
#       
#       # Filter
#       dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
#                  !is.na(biomass) & biomass > 0 &
#                  !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
#       
#       if(nrow(dt) > 0) {
#         dt[, hex_ID := hex_sf$hex_ID[idx]]
#         group_results[[i]] <- dt
#       }
#     }, error = function(e) NULL)
#   }
#   
#   return(rbindlist(group_results[!sapply(group_results, is.null)]))
# }
# 
# # Esegui in parallelo
# cat("Processing", nrow(Hex_EU), "hexagons in parallel on", n_cores, "cores...\n")
# 
# all_results <- future_lapply(
#   hex_groups,
#   process_hex_group,
#   hex_sf = Hex_EU,
#   raster_stack = eu_stack,
#   future.seed = TRUE
# )
# 
# # Combina i risultati
# final_dt <- rbindlist(all_results)
# 
# # Chiudi parallelizzazione
# plan(sequential)
# 
# 
# # alternatives2
# 
# library(exactextractr)
# library(data.table)
# 
# # Converti eu_stack in raster di raster package se necessario
# # o usa direttamente con terra
# 
# # Estrai tutto in una volta (molto più veloce)
# biomass_vals <- exact_extract(Biomass2010_aligned, Hex_EU, include_cols = "hex_ID")
# undisturbed_vals <- exact_extract(undisturbed_eu_vrt, Hex_EU, include_cols = "hex_ID")
# forest_type_vals <- exact_extract(EEA_forest_type_aligned, Hex_EU, include_cols = "hex_ID")
# 
# # Combina e filtra
# process_exact_extract <- function(i) {
#   dt <- data.table(
#     biomass = biomass_vals[[i]]$value,
#     undisturbed = undisturbed_vals[[i]]$value,
#     forest_type = forest_type_vals[[i]]$value,
#     hex_ID = Hex_EU$hex_ID[i]
#   )
#   
#   dt <- dt[!is.na(undisturbed) & undisturbed == 1 & 
#              !is.na(biomass) & biomass > 0 &
#              !is.na(forest_type) & forest_type %in% c(1, 2, 3)]
#   
#   return(dt)
# }
# 
# all_results <- lapply(1:nrow(Hex_EU), process_exact_extract)
# final_dt <- rbindlist(all_results[!sapply(all_results, is.null)])
# 




# Combine results
biomass_hex_dt <- rbindlist(all_results[!sapply(all_results, is.null)])

# Add forest type labels
biomass_hex_dt[, forest_type_label := fcase(
  forest_type == 1, "Broadleaf",
  forest_type == 2, "Needleleaf",
  forest_type == 3, "Mixed Forest",
  default = NA_character_
)]

# Save
fwrite(biomass_hex_dt, "Data/biomass_EU_by_hexagon_undisturbed.csv")

cat("\nTotal valid pixels:", nrow(biomass_hex_dt), "\n")
cat("Hexagons with data:", length(unique(biomass_hex_dt$hex_ID)), "\n")




# OPTION PARALLEL PROCESSING (uncomment to use)
# library(future.apply)
# plan(multisession, workers = 4)  # Use 4 cores
# 
# all_results <- future_lapply(1:nrow(Hex_EU), function(i) {
#   extract_hex_data(Hex_EU$hex_ID[i], Hex_EU[i, ], eu_stack)
# }, future.seed = TRUE)
