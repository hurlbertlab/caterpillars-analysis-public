### Combine iNat moths and caterpillars, get species by grid cell

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)
library(zoo)
library(mgcv)
library(raster)

# Read in data

inat_moths <- read.csv("data/inat_moths.csv", header = T)

inat_cats <- read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

cats <- inat_cats %>%
  left_join(inat_species, by = "scientific_name") %>%
  filter(family %in% c("Erebidae", "Geometridae", "Noctuidae", "Notodontidae")) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  mutate(life_stage = "caterpillar")

inat_adults <- inat_moths %>%
  filter(!(id %in% inat_cats$id)) %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  mutate(life_stage = "adult")

# Combine adult and caterpillar datasets

all_inat <- cats %>%
  dplyr::select(id, observed_on, place_guess, latitude, longitude, scientific_name,
                common_name, taxon_id, family, 
                Date, year, jday, lat_bin, lon_bin, jd_wk, life_stage) %>%
  rename(taxon_family_name = "family") %>%
  bind_rows(dplyr::select(inat_adults, -place_county_name))
write.csv(all_inat, "data/inat_adults_cats_ENA.csv", row.names = F)

# Group obs by cell, year, life_stage, species

species_inat <- all_inat %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  filter(n_distinct(id) >= 50) %>%
  group_by(year, lat_bin, lon_bin, life_stage, scientific_name) %>%
  count() %>%
  filter(n >= 10, year >= 2017, lat_bin >  24)
write.csv(species_inat, "data/inat_adults_cats_ENA_spp_counts.csv", row.names = F)
