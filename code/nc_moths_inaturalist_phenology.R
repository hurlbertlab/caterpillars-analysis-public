### Modeling phenology across multiple data sources

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)

###### Caterpillars: iNaturalist vs. Caterpillars Count

# This is located in code/compare_inat_cc.R

###### Adult moths: NC Moths vs. iNaturalist #########

# iNaturalist, four families, combine
# Filter out caterpillar records 

inat_cats = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

# NC Moths

mnc_species <- read.table("data/mnc_species_complete.txt", header = T)
mnc <- read.csv("\\\\BioArk\\HurlbertLab\\Databases\\NC Moths\\moth_records_thru2018_lineCleaned.txt", header=T, sep = ';', stringsAsFactors = F)

mnc_filtered <- mnc %>%
  left_join(mnc_species, by = c("sciName" = "sci_name")) %>%
  filter(family == "Geometridae" | family == "Erebidae" | family == "Noctuidae" | family == "Notodontidae") %>%
  mutate(year = year(as.Date(date)), jday = yday(as.Date(date))) %>%
  filter(year == 2018)

# Map of data records at county level for four major woody moth families

NC <- st_read("C:/Users/gdicecco/Desktop/CountyBoundary/BoundaryCountyPolygon.shp") %>%
  dplyr::select(CountyName, geometry)

mnc_survey_effort <- mnc_filtered %>%
  group_by(county) %>%
  summarize(moths = sum(as.numeric(number))) %>%
  rename("CountyName" = "county")

mnc_map <- merge(NC, mnc_survey_effort, by = "CountyName")

mnc_tm <- tm_shape(mnc_map) + tm_fill(col = "moths", palette = "BuGn") + tm_shape(NC) + tm_borders() +
  tm_layout(title = "2018")
tmap_save(mnc_tm, "figs/NC_moths_countyMap.pdf")

# Phenology curves for these two, group by 1 degree lat lon bins

# Raw correlations of phenology

# Correlation of deltas

###### Caterpillars <-> Adults: iNaturalist vs. iNaturalist ########

# Phenology curves, group by 2 degree lat lon bins