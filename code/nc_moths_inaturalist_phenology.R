### Modeling phenology across multiple data sources

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)

# iNaturalist, four families, combine
# Filter out caterpillar records 

inat_moths <- read.csv("data/inat_moths.csv", header = T)

inat_cats = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

inat_species <- read.table("data/inat_species.txt", header = T, sep = "\t", quote = '\"')

# Remove caterpillar observations
inat_adults <- inat_moths %>%
  filter(!(id %in% inat_cats$id))

###### Caterpillars: iNaturalist vs. Caterpillars Count

# This is located in code/compare_inat_cc.R

###### Adult moths: NC Moths vs. iNaturalist #########

# NC Moths

mnc_species <- read.table("data/mnc_species_complete.txt", header = T)
mnc <- read.csv("\\\\BioArk\\HurlbertLab\\Databases\\NC Moths\\moth_records_thru2018_lineCleaned.txt", header=T, sep = ';', stringsAsFactors = F)

# Map of data records at county level for four major woody moth families

NC <- st_read("data/CountyBoundary/BoundaryCountyPolygon.shp") %>%
  dplyr::select(CountyName, geometry)
NC_geog = st_transform(NC, "+proj=longlat +datum=WGS84")
NC_centroids = st_centroid(NC_geog) %>% 
  st_coordinates() %>%
  data.frame() %>%
  mutate(CountyName = NC$CountyName)



mnc_filtered <- mnc %>%
  left_join(mnc_species, by = c("sciName" = "sci_name")) %>%
  filter(family == "Geometridae" | family == "Erebidae" | family == "Noctuidae" | family == "Notodontidae") %>%
  mutate(year = year(as.Date(date)), jday = yday(as.Date(date))) %>%
  left_join(NC_centroids, by = c('county' = 'CountyName')) %>%
  rename('lat_old' = 'lat', 'lon_old' = 'lon', 'lat' = 'Y', 'lon' = 'X') %>%
  filter(year == 2018)



mnc_survey_effort <- mnc_filtered %>%
  group_by(county) %>%
  summarize(moths = sum(as.numeric(number))) %>%
  rename("CountyName" = "county")

mnc_map <- merge(NC, mnc_survey_effort, by = "CountyName")

mnc_tm <- tm_shape(mnc_map) + tm_fill(col = "moths", palette = "BuGn") + tm_shape(NC) + tm_borders() +
  tm_layout(title = "2018")
tmap_save(mnc_tm, "figs/NC_moths_countyMap.pdf")

# NC iNaturalist obs

nc_inat <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == 2018) %>%
  filter(latitude > 33, latitude < 38, longitude > -83, longitude < -75) %>%
  st_as_sf(coords = c("longitude", "latitude"))

inat_nc_tmap <- tm_shape(NC) + tm_borders() + tm_shape(nc_inat) + tm_dots(col = "jday")
tmap_save(inat_nc_tmap, "figs/NC_iNat_adultMoths_Map.pdf")

# Phenology curves for these two, group by 1 degree lat lon bins

nc_bins <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == 2018) %>%
  filter(latitude > 33, latitude < 38, longitude > -83, longitude < -75) %>%
  mutate(lat_bin = round(latitude, 0),
         lon_bin = round(longitude, 0),
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, jd_wk) %>%
  summarize(iNat_moths = n())

mnc_bins <- mnc_filtered %>%
  mutate(lat_bin = round(lat, 0),
         lon_bin = round(lon, 0),
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, jd_wk) %>%
  summarize(MNC_moths = sum(as.numeric(number), na.rm = T))

nc_moths_spread <- mnc_bins %>%
  full_join(nc_bins, by = c("lat_bin", "lon_bin", "jd_wk"))

nc_moths_comb <- mnc_bins %>%
  full_join(nc_bins, by = c("lat_bin", "lon_bin", "jd_wk")) %>%
  gather(key = 'data_source', value = "nMoths", MNC_moths, iNat_moths)

# Raw correlations of phenology

ggplot(filter(nc_moths_comb, lat_bin %in% c(35, 36)), aes(x = jd_wk, y = nMoths, col = data_source)) +
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d(begin = 0.5) +
  scale_y_log10() +
  facet_grid(lat_bin~lon_bin) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figs/phenocurves_iNat_NCMoths.pdf")

ggplot(nc_moths_spread, aes(x = iNat_moths, y = MNC_moths)) +
  geom_point() + 
  geom_abline(yintercept = 0, xintercept = 0, slope = 1, intercept= 0)
ggsave("figs/pheno_correlations_iNat_NCMoths_Asheville.pdf")

# Correlation of deltas

nc_moth_deltas <- nc_moths_spread %>%
group_by(lat_bin, lon_bin) %>%
  nest() %>%
  mutate(deltasDF = purrr::map(data, ~{
    df <- .
    len <- nrow(df) - 1
    deltaJD <- c()
    deltaNC <- c()
    deltaiNat <- c()
    for(i in 1:len) {
      JD <- df[i + 1, 1]$jd_wk - df[i, 1]$jd_wk
      NC <- df[i + 1, 2]$MNC_moths - df[i, 2]$MNC_moths
      iNat <- df[i + 1, 3]$iNat_moths - df[i, 3]$iNat_moths
      
      deltaJD <- c(deltaJD, JD)
      deltaNC <- c(deltaNC, NC)
      deltaiNat <- c(deltaiNat, iNat)
    }
    data.frame(deltaJD = deltaJD, deltaNC = deltaNC, deltaiNat = deltaiNat)
  })) %>%
  dplyr::select(lat_bin, lon_bin, deltasDF) %>%
  unnest()


ggplot(nc_moth_deltas, aes(x = deltaNC, y = deltaiNat)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(lat_bin ~ lon_bin) + theme_bw()
ggsave("figs/pheno_deltas_NCMoths_iNat.pdf")

ggplot(filter(nc_moth_deltas, lat_bin == 36, lon_bin == -82), aes(x = deltaNC, y = deltaiNat)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) 
ggsave("figs/pheno_deltas_NCMoths_iNat_Asheville.pdf")

###### Caterpillars <-> Adults: iNaturalist vs. iNaturalist ########

adult_moths <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == 2018) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, jd_wk) %>%
  summarize(moths = n())

caterpillars <- inat_cats %>%
  left_join(inat_species, by = "scientific_name") %>%
  filter(family %in% c("Erebidae", "Geometridae", "Noctuidae", "Notodontidae")) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == 2018) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, jd_wk) %>%
  summarize(caterpillars = n())

inat_combined <- adult_moths %>%
  full_join(caterpillars, by = c("lat_bin", "lon_bin", "jd_wk")) %>%
  filter(lat_bin > 24)

inat_combined_gather <- inat_combined %>%
  gather(key = 'life_stage', value = "nObs", moths, caterpillars)

# Phenology curves, group by 2 degree lat lon bins

# Raw correlations of phenology

ggplot(inat_combined_gather, aes(x = jd_wk, y = nObs, col = life_stage)) +
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d(begin = 0.5) +
  scale_y_log10() +
  facet_grid(forcats::fct_reorder(factor(lat_bin), desc(lat_bin))~lon_bin) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figs/phenocurves_iNat_mothsAndCaterpillars.pdf", units = "in", height = 20, width = 24)
