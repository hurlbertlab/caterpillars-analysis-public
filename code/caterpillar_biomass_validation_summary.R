## Caterpillar biomass estimates: model validation with iNat

library(tidyverse)
library(lubridate)
library(tmap)
library(sf)

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

# ~10-15% identified to species but not research grade
table(cats$year, cats$quality_grade)

cats_overlap <- cats %>%
  filter(scientific_name %in% spplist$V1)

spp_overlap <- unique(cats_overlap$scientific_name)
overlap_density <- cats_overlap %>%
  group_by(scientific_name) %>%
  count()
# Monarchs @ 9500 records, everything else below 200 records
# Fall armyworm Spodoptera frugiperda 150, Cabbage white Pieris rapae 120

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
 