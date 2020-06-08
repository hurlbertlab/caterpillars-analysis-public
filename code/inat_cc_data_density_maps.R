#### iNaturalist and Caterpillars Count data density maps

## Libraries

library(tidyverse)
library(dbplyr)
library(RSQLite)
library(maps)
library(rgdal)
library(sf)
library(tmap)
library(forcats)
library(dggridR)

#####  Read in data ########
inat = read.csv('data/inat_caterpillars_eastern_NA_5-20-2020.csv', header = TRUE, stringsAsFactors = F)

NAmap = read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/inat_caterpillar_species_traits.txt", header = T, sep = "\t")

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# hex grid

hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")

## Get June arthropod records from all iNaturalist database to control for sampling effort iNat

# For 2019
setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/inat_thru_2019")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inat_2019.db")

db_list_tables(con)

inat_insects_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7))

inat_insects_df <- inat_insects_db %>%
  collect()

# For earlier

setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

inat_insects_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7))


inat_insects_early_df <- inat_insects_db %>%
  collect()

## iNat insect number of observations per hex cell

inat_june_obs <- inat_insects_early_df %>%
  mutate(year_num = as.numeric(year)) %>%
  filter(year_num > 2015, year_num < 2019) %>%
  bind_rows(inat_insects_df) %>%
  filter(month == "06") %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  group_by(year, cell) %>%
  summarize(n_obs = n_distinct(id))

inat_june_hex_obs <- inat_june_obs %>%
  st_set_geometry(NULL)
# write.csv(inat_june_hex_obs, "data/inat_insect_obs_june2016-2019_hex.csv", row.names = F)

# Only map effort for iNat in hex cells with at least 100 arthropod observations
inat_june_hex_obs <- read.csv("data/inat_insect_obs_june2016-2019_hex.csv") %>%
  filter(n_obs >= 100)

## Caterpillars count data density in June: sites with at least 40 branches, calc % surveys with caterpillars and # cats per survey

outlierCount = 10000

catcount_june <- fullDataset %>%
  mutate(month = word(LocalDate, 2, sep = "-")) %>%
  filter(month == "06") %>%
  mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
  group_by(Name, Region, Latitude, Longitude, Year) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(Quantity2, na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0])),
            totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
  filter(nSurveyBranches >= 40) %>%
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys) %>%
  filter(Year >= 2016 & Year <= 2019)

## iNat data density in June: caterpillar obs/total arth obs

four_families <- c("Erebidae", "Noctuidae", "Notodontidae", "Geometridae")

inat_june <- inat %>%
  mutate(year = as.numeric(word(observed_on, 1, sep = "-")),
         month = as.numeric(word(observed_on, 2, sep = "-"))) %>%
  filter(year >= 2016, year <= 2019, month == 6, user_login != "caterpillarscount") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  group_by(year, cell) %>%
  summarize(n_cats = n_distinct(id),
            n_cats_4fam = n_distinct(id[taxon_family_name %in% four_families])) %>%
  right_join(inat_june_hex_obs, by = c("year", "cell")) %>%
  mutate(cats_effort = n_cats/n_obs,
         cats_4fam_effort = n_cats_4fam/n_obs) %>%
  st_set_geometry(NULL)

inat_june_hex <- hex %>%
  right_join(inat_june)

## Figure: iNat hex density with CC location dots, All and just 4 common families

easternNA <- NAmap %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 55)) %>%
  st_transform("+proj=ortho +lon_0=-75 +lat_0=40")

inat_june_ortho <- inat_june_hex %>%
  st_transform(st_crs(easternNA)) %>%
  filter(!is.na(cats_effort), cats_effort > 0) %>%
  mutate(log_cats_effort = log10(cats_effort))

catcount_june_ortho <- catcount_june %>%
  rename(year = "Year") %>%
  st_as_sf(coords= c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 55)) %>%
  st_transform(st_crs(easternNA))
  
all_cats <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(inat_june_ortho) + tm_polygons(col = "cats_effort", palette = "YlGnBu", title = "Caterpillar obs.", alpha = 0.65) + tm_facets(by = "year", nrow = 1) +
  tm_shape(catcount_june_ortho) + tm_dots(col = "black", size = 0.25) + tm_facets(by = "year", nrow = 1) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.5, panel.label.size = 1.5) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count! site"), shape = 16, size = 0.75)

inat_june_fourfams <- inat_june_ortho %>%
  filter(!is.na(cats_4fam_effort), cats_4fam_effort > 0)

four_fams <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(inat_june_fourfams) + tm_polygons(col = "cats_4fam_effort", palette = "YlGnBu", title = "Woody Caterpillar obs.", alpha = 0.65) + tm_facets(by = "year", nrow = 1) +
  tm_shape(catcount_june_ortho) + tm_dots(col = "black", size = 0.25) + tm_facets(by = "year", nrow = 1) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.5, panel.label.size = 1.5) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count! site"), shape = 16, size = 0.75)

inat_cc <- tmap_arrange(all_cats, four_fams, nrow = 2)
tmap_save(inat_cc, "figs/cross-comparisons/inat_cat_with_CC_june_map.pdf", units = "in", width = 12, height = 6)

## Figure: CC data, % surveys with caterpillars, # cats per survey

cc_fracsurveys <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(catcount_june_ortho) + tm_dots(col = "fracSurveys", size = 0.25, palette = "YlGnBu", title = "Pct. surveys with caterpillars") + 
  tm_facets(by = "year", nrow = 1, free.coords = F) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.5, panel.label.size = 1.5) 

cc_meandens <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(catcount_june_ortho) + tm_dots(col = "meanDensity", size = 0.25, palette = "YlGnBu", title = "Avg. caterpillars per survey") + 
  tm_facets(by = "year", nrow = 1, free.coords = F) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.5, panel.label.size = 1.5) 

cc_map <- tmap_arrange(cc_fracsurveys, cc_meandens, nrow = 2)
tmap_save(cc_map, "figs/caterpillars-count/catcount_june_map.pdf", units = "in", width = 12, height = 6)
