## Caterpillars survey density in hex cells, 1 degree grid cells

library(tidyverse)
library(dggridR)
library(sf)

# Get CC dataset
source("code/analysis_functions.R")

# Read in hex data
hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp")

# Number of weeks per site per year per hex cell
cc_sf <- fullDataset %>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersection(hex)

cc_hex_density <- cc_sf %>%
  group_by(cell, Year, Name) %>%
  summarize(sampling_weeks = n_distinct(julianweek))

cc_hex_density_df <- cc_hex_density %>%
  st_set_geometry(NULL)
# write.csv(cc_hex_density_df, "data/derived_data/CatCount_density_hex_cells.csv", row.names = F)
  
# Number of weeks per site per year per 1x1 degree grid cell

