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
library(grid)

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
  dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7))

inat_insects_df <- inat_insects_db %>%
  collect()

DBI::dbDisconnect(con, "inat")

inat_2019_june <- inat_insects_df %>%
  filter(year == "2019", month == "06")
# 75,891 observations

# For earlier

setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

inat_insects_early_db <- tbl(con, "inat") %>%
  dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7))

inat_insects_early_df <- inat_insects_early_db %>%
  collect()

inat_2018_june <- inat_insects_early_df %>%
  filter(year == "2018", month == "06")
# 172,043 obs

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
  filter(n_obs >= 100) %>%
  filter(year == 2019)

## Caterpillars count data density in June: sites with at least 40 branches, calc % surveys with caterpillars and # cats per survey

outlierCount = 10000

catcount_june <- fullDataset %>%
  mutate(month = word(LocalDate, 2, sep = "-")) %>%
  filter(month == "06") %>%
  filter(Region != "IA") %>%
  mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
  mutate(groupName = case_when(grepl("EwA", Name) ~ "EwA",
                               grepl("Acadia", Name) ~ "Acadia",
                               grepl("Litzsinger", Name)~ "Litzsinger",
                               TRUE ~ Name)) %>%
  group_by(groupName, Region, cell, Latitude, Longitude, Year) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(Quantity2[Group == "caterpillar"], na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0 & Group == "caterpillar"])),
            totalBiomass = sum(Biomass_mg[Group == "caterpillar"], na.rm = TRUE)) %>% 
  filter(nSurveys >= 40) %>%
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys) %>%
  filter(Year == 2019)

## iNat data density in June: caterpillar obs/total arth obs

woody_families <- c("Erebidae", "Noctuidae", "Notodontidae", "Geometridae", "Depressariidae", "Tortricidae")

inat_june <- inat %>%
  mutate(year = as.numeric(word(observed_on, 1, sep = "-")),
         month = as.numeric(word(observed_on, 2, sep = "-"))) %>%
  filter(year == 2019, month == 6, user_login != "caterpillarscount") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  group_by(cell) %>%
  summarize(n_cats = n_distinct(id),
            n_cats_4fam = n_distinct(id[taxon_family_name %in% woody_families])) %>%
  right_join(inat_june_hex_obs, by = c("cell")) %>%
  mutate(cats_effort = n_cats/n_obs,
         cats_4fam_effort = n_cats_4fam/n_obs) %>%
  st_set_geometry(NULL)

inat_june_hex <- hex %>%
  right_join(inat_june)

# Identified CC observations, June

surveys_per_cell <- fullDataset %>%
  mutate(month = word(LocalDate, 2, sep = "-")) %>%
  filter(month == "06", Year == 2019) %>%
  filter(Region != "IA") %>%
  group_by(cell) %>%
  summarize(nSurveys = n_distinct(ID)) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

cc_id_june <- inat %>%
  mutate(year = as.numeric(word(observed_on, 1, sep = "-")),
         month = as.numeric(word(observed_on, 2, sep = "-"))) %>%
  filter(year == 2019, month == 6, user_login == "caterpillarscount") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  left_join(surveys_per_cell) %>%
  group_by(year, cell, nSurveys) %>%
  summarize(n_cats = n_distinct(id)/mean(nSurveys),
            n_cats_4fam = n_distinct(id[taxon_family_name %in% woody_families])/mean(nSurveys)) %>%
  st_set_geometry(NULL)

cc_id_june_hex <- hex %>%
  right_join(cc_id_june)

## Figure: iNat/CC hex density, All and just 4 common families, 1:1 plot

easternNA <- NAmap %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform("+proj=ortho +lon_0=-75 +lat_0=40")

inat_june_ortho <- inat_june_hex %>%
  st_transform(st_crs(easternNA)) %>%
  filter(!is.na(cats_effort), cats_effort > 0)

cc_id_june_ortho <- cc_id_june_hex %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform(st_crs(easternNA))

cc_id_june_sites <- inat %>%
  mutate(year = as.numeric(word(observed_on, 1, sep = "-")),
         month = as.numeric(word(observed_on, 2, sep = "-"))) %>%
  filter(year == 2019, month == 6, user_login == "caterpillarscount") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform(st_crs(easternNA))

all_cats <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(inat_june_ortho) + tm_polygons(col = "cats_effort", palette = "YlGnBu", title = "Caterpillars", alpha = 0.65)+
  tm_layout(legend.text.size = 1, legend.title.size = 1.3, outer.margins = c(0.01,0,0.01,0), title = "iNaturalist")

inat_june_fourfams <- inat_june_ortho %>%
  filter(!is.na(cats_4fam_effort), cats_4fam_effort > 0)

four_fams <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(inat_june_fourfams) + tm_polygons(col = "cats_4fam_effort", palette = "YlGnBu", title = "Woody caterpillars", alpha = 0.65)+
  tm_layout(legend.text.size = 1, legend.title.size = 1.3, outer.margins = c(0.01,0,0.01,0), title = "iNaturalist")

cc_all_cats <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(cc_id_june_ortho) + tm_polygons(col = "n_cats", palette = "YlGnBu", title = "Caterpillars", alpha = 0.65) + 
  tm_shape(cc_id_june_sites) + tm_dots(col = "black", size = 0.067) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count!"), shape = 16, size = 0.5)+
  tm_layout(legend.text.size = 1, legend.title.size = 1.3, outer.margins = c(0.01,0,0.01,0), title = "Caterpillars Count!")

cc_four_fams <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(cc_id_june_ortho) + tm_polygons(col = "n_cats_4fam", palette = "YlGnBu", title = "Woody caterpillars", alpha = 0.65) + 
  tm_shape(cc_id_june_sites) + tm_dots(col = "black", size = 0.067) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count!"), shape = 16, size = 0.5) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.3, outer.margins = c(0.01,0,0.01,0), title = "Caterpillars Count!")

cc_inat_11 <- inat_june %>%
  right_join(cc_id_june, by = c("year", "cell"), suffix = c("_inat", "_cc"))

theme_set(theme_classic(base_size = 17))
inat_cc_11_plot <- ggplot(cc_inat_11, aes(x = cats_effort, y = n_cats_cc)) + geom_point(cex = 2) + scale_y_log10() +
  geom_smooth(method = "lm", col = "darkgray", se = F, cex = 1.5) +
  labs(x = "iNaturalist caterpillar density", y = "Caterpillars Count! caterpillar density")

inat_cc_11_4fam_plot <- ggplot(cc_inat_11, aes(x = cats_4fam_effort, y = n_cats_4fam_cc)) + geom_point(cex = 2) + scale_y_log10() +
  geom_smooth(method = "lm", col = "darkgray", se = F, cex = 1.5) +
  labs(x = "iNaturalist woody caterpillar density", y = "Caterpillars Count! woody caterpillar density")

grid.newpage()
pdf(paste0(getwd(),"/figs/cross-comparisons/inat_cc_data_density_six_panel.pdf"), height = 10, width = 18)
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 3)))
print(all_cats, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(four_fams, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(cc_all_cats, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(cc_four_fams, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(inat_cc_11_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(inat_cc_11_4fam_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
dev.off()

## Figure: CC data, % surveys with caterpillars, # cats per survey

catcount_avgs_june <- catcount_june %>%
  rename(year = "Year") %>%
  st_as_sf(coords= c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  group_by(year, cell) %>%
  summarize(meanFracSurveys = mean(fracSurveys),
            meanMeanDensity = mean(meanDensity),
            meanMeanBiomass = mean(meanBiomass)) %>%
  st_set_geometry(NULL)

cc_avgs_june_hex <- hex %>%
  right_join(catcount_avgs_june) %>%
  st_transform(st_crs(easternNA))

easternNA_zoom <- NAmap %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  st_crop(c(xmin = -93, ymin = 20, xmax = -59, ymax = 48)) %>%
  st_transform("+proj=ortho +lon_0=-75 +lat_0=40")

catcount_sites_june_ortho <- catcount_june %>%
   rename(year = "Year") %>%
   st_as_sf(coords= c("Longitude", "Latitude")) %>%
   st_set_crs(4326) %>%
   st_crop(c(xmin = -93, ymin = 20, xmax = -59, ymax = 48)) %>%
   st_transform(st_crs(easternNA))

# Quantile color breaks
breaks_fracsurveys <- quantile(cc_avgs_june_hex$meanFracSurveys)
breaks_meandens <- quantile(cc_avgs_june_hex$meanMeanDensity)
breaks_meanbiomass <- quantile(cc_avgs_june_hex$meanMeanBiomass)
  
cc_fracsurveys <- tm_shape(easternNA_zoom) + tm_polygons() +
  tm_shape(cc_avgs_june_hex) + 
  tm_polygons(col = "meanFracSurveys", size = 0.25, palette = "YlGnBu", title = "% surveys with caterpillars", 
              alpha = 0.65, breaks = breaks_fracsurveys) +
  tm_shape(catcount_sites_june_ortho) + tm_dots(col = "black", size = 0.05) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count!"), shape = 16, size = 0.5) +
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5)

cc_meandens <- tm_shape(easternNA_zoom) + tm_polygons() +
  tm_shape(cc_avgs_june_hex) + 
  tm_polygons(col = "meanMeanDensity", size = 0.25, palette = "YlGnBu", title = "Caterpillars/survey", 
              alpha = 0.65, breaks = breaks_meandens) + 
  tm_shape(catcount_sites_june_ortho) + tm_dots(col = "black", size = 0.05) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count!"), shape = 16, size = 0.5) +
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5)

cc_meanbiomass <- tm_shape(easternNA_zoom) + tm_polygons() +
  tm_shape(cc_avgs_june_hex) + 
  tm_polygons(col = "meanMeanBiomass", size = 0.25, palette = "YlGnBu", title = "Biomass (mg)/survey", 
              alpha = 0.65, breaks = breaks_meanbiomass) + 
  tm_shape(catcount_sites_june_ortho) + tm_dots(col = "black", size = 0.05) +
  tm_add_legend(type = c("symbol"), col = "black", labels = c("Caterpillars Count!"), shape = 16, size = 0.5) +
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5) 

cc_map <- tmap_arrange(cc_fracsurveys, cc_meandens, cc_meanbiomass, ncol = 3)
tmap_save(cc_map, "figs/caterpillars-count/catcount_june_map.pdf", units = "in", width = 18, height = 6)
