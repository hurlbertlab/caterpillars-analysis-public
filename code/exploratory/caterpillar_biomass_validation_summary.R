## Caterpillar biomass estimates: model validation with iNat

library(tidyverse)
library(lubridate)
library(tmap)
library(sf)

# Spatial extent

# 1 degree cells:
#   MA region: 42-43 North, 70.5-71.5 West
# MD region: 38.5-39.5 North, 76.5-77.5 West
# GA region: 33.5-34.5 North, 83.25-84.25 West

one_deg_bins <- function(lat, lon) {
  case_when(lat >= 42 & lat <= 43 & lon <= -70.5 & lon >= -71.5 ~ "MA",
            lat >= 38.5 & lat <= 39.5 & lon <= -76.5 & lon >= -77.5 ~ "MD",
            lat >= 33.5 & lat <= 34.5 & lon <= -83.25 & lon >= -84.25 ~ "GA")
}
 
# Flatter cells:
#   MA region: 42.25-42.75 N, 70.5-72.5 W  
# MD region: 38.5-39 N, 76-78 W
# GA region: 33.6-34.1 N, 83-85 W

flat_bins <- function(lat, lon) {
  case_when(lat >= 42.25 & lat <= 42.75 & lon <= -70.5 & lon >= -72.5 ~ "MA",
            lat >= 38.5 & lat <= 39.5 & lon <= -76 & lon >= -78 ~ "MD",
            lat >= 33.6 & lat <= 34.1 & lon <= -83 & lon >= -85 ~ "GA")
}

# Temp database species list

spplist <- read.csv("C:/Users/gdicecco/Desktop/SpeciesList.csv", header = F)

# Caterpillar data

inatcat = read.csv('data/inat_caterpillars_easternNA.csv', header = T, quote = '\"', fill = TRUE, stringsAsFactors = FALSE)

families <- unique(inatcat$taxon_family_name)
genera <- unique(inatcat$taxon_genus_name)

cats <- inatcat %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date)) %>%
  filter(!(scientific_name %in% families),
         !(scientific_name %in% genera)) %>%
  mutate(genus = word(scientific_name, start = 1, sep = " "),
         species = word(scientific_name, start = 2, sep = " ")) %>%
  filter(!is.na(species),
         year >= 2015, 
         !is.na(latitude),
         latitude > 25)

# Number of records per species in the bins required 

cats_onedeg_bins <- cats %>%
  mutate(region = one_deg_bins(latitude, longitude)) %>%
  filter(!is.na(region)) %>%
  group_by(region, year, scientific_name) %>%
  summarize(num_obs = n_distinct(id),
            research_grade = length(id[quality_grade == "research"])/num_obs) %>%
  filter(num_obs >= 5)

overlap_spp <- filter(cats_onedeg_bins, scientific_name %in% spplist$V1)

# write.csv(cats_onedeg_bins, "data/inat_caterpillars_biomass_onedeg.csv", row.names = F)

cats_flat_bins <- cats %>%
  mutate(region = flat_bins(latitude, longitude)) %>%
  filter(!is.na(region)) %>%
  group_by(region, year, scientific_name) %>%
  summarize(num_obs = n_distinct(id),
            research_grade = length(id[quality_grade == "research"])/num_obs) %>%
  filter(num_obs >= 5)

# write.csv(cats_flat_bins, "data/inat_caterpillars_biomass_flatbin.csv", row.names = F)

overlap_spp_flat <- filter(cats_flat_bins, scientific_name %in% spplist$V1)

# Cat species data for 
# Acronicta americana
# Malacosoma americana
# Papilio polyxenes

threespp <- c("Acronicta americana", "Malacosoma americana", "Papilio polyxenes")

cat_dates_onedeg <- cats %>%
  mutate(region = one_deg_bins(latitude, longitude),
         jday = yday(Date)) %>%
  filter(!is.na(region), scientific_name %in% threespp) %>%
  select(id, observed_on, url, description, quality_grade, latitude, longitude, scientific_name, year, jday, region)

# write.csv(cat_dates_onedeg, "C:/Users/gdicecco/Desktop/three_cats_obs_onedeg.csv", row.names = F)

cat_dates_flat <- cats %>%
  mutate(region = flat_bins(latitude, longitude),
         jday = yday(Date)) %>%
  filter(!is.na(region), scientific_name %in% threespp) %>%
  select(id, observed_on, url, description, quality_grade, latitude, longitude, scientific_name, year, jday, region)

# write.csv(cat_dates_flat, "C:/Users/gdicecco/Desktop/three_cats_obs_flatbin.csv", row.names = F)

# Caterpillar data density by 1 degree lat lon grids

cats_grid <- cats %>%
  mutate(lat_bin = round(latitude),
         lon_bin = round(longitude)) %>%
  group_by(year, lat_bin, lon_bin) %>%
  count() %>%
  mutate(log_n = log10(n))

NA_sf <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

easternUS <- st_bbox(c(xmin = -110, xmax = -50, ymin = 25, ymax = 61))

na_crop <- NA_sf %>%
  st_crop(easternUS)

US_map <- tm_shape(na_crop) + tm_borders(col = "grey70") + tm_fill(col = "grey80")

cats_sf <- cats_grid %>%
  st_as_sf(coords = c("lon_bin", "lat_bin")) %>%
  st_crop(easternUS)

cat_map <- US_map + tm_shape(cats_sf) + tm_dots(col = "log_n", title = "Log10(Obs)", palette = "Blues") +
  tm_facets(by = "year") + tm_layout(scale = 2)
tmap_save(cat_map, "figs/inaturalist/cat_to_spp_density_map.pdf", height = 6, width = 12, units = "in")
 