### iNaturalist user behavior MS
## Figures and analysis

## Earlier iterations of plot code in inat_user_behavior.R
## Clean data for plots from iNat databases in inat_ms_data_cleaning.R
## Save figures to caterpillars-analysis-public/figs/inaturalist

#### Libraries ####

library(tidyverse)
library(cowplot)
library(sf)
library(rnaturalearth)
library(tmap)
library(maptools)

## Load World GIS Boundareis
data(wrld_simpl)

world <-  wrld_simpl %>% 
  st_as_sf()

## load inat lat and long of observations
inat_records <- data.table::fread("iNatUserBehavior/bigdata/inat_thru_2019_sites.csv") %>% 
  na.omit()

inat_sf <- st_as_sf(inat_records, coords = c("longitude", "latitude"), crs = 4326)  

inat_by_countries <-  st_join(inat_sf, world, 
                              join=st_intersects)


inat_by_countries <- inat_by_countries %>% 
  dplyr::select(NAME, POP2005, LON, LAT, geometry)

saveRDS(object = inat_by_countries[16000001:23714390,], file = "iNatUserBehavior/bigdata/inat_stjoin3.rds")

inat_groupedBy_countries <- inat_by_countries %>% 
  group_by(NAME, geometry) %>% 
  summarise(count = n())

saveRDS(inat_by_countries, file = "bigdata/inat_records_by_countries.rds")
''