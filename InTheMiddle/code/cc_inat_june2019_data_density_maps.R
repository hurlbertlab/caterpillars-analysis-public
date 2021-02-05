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

inat_species = read.table("data/taxonomy/inat_caterpillar_species_traits.txt", header = T, sep = "\t")

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# hex grid

hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")

## Get June insect records from all iNaturalist database to control for sampling effort iNat
### Use June 2019 iNat CSV data

# Append correct BioArk path
info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\BioArk")

inat_june_2019 <- read.csv(paste0(bioark, "/HurlbertLab/Databases/iNaturalist/inat_june_2019/inat_2019_06.csv"), stringsAsFactors = F)

## iNat insect number of observations per hex cell

inat_june_obs <- inat_june_2019 %>%
  dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(user_login != "caterpillarscount") %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  mutate(year_num = as.numeric(year)) %>%
  filter(year_num == 2019) %>%
  filter(month == "06") %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(hex)) %>%
  st_intersection(hex) %>%
  group_by(year, cell) %>%
  summarize(n_obs = n_distinct(id))

inat_june_hex_obs <- inat_june_obs %>%
  filter(n_obs >= 100) %>%
  st_set_geometry(NULL)

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
# total number of caterpillars
sum(inat_june$n_cats, na.rm = T)

inat_june_hex <- hex %>%
  right_join(inat_june)

# Identified CC observations, June

surveys_per_cell <- fullDataset %>%
  mutate(month = word(LocalDate, 2, sep = "-")) %>%
  filter(month == "06", Year == 2019) %>%
  filter(Region != "IA") %>%
  group_by(cell) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK, julianday),
            nSurveys = n_distinct(ID),
            nCats = sum(Quantity[Group == "caterpillar"], na.rm = T)) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))
# Total surveyed branches
sum(surveys_per_cell$nSurveyBranches)
# Total caterpillars
sum(surveys_per_cell$nCats)

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
  tm_shape(inat_june_ortho) + tm_polygons(col = "cats_effort", palette = "YlGnBu", title = "Occurrence", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, outer.margins = c(0.01,0,0.01,0), 
            inner.margins = c(0.02, 0.02, 0.1, 0.1), title = "A. iNaturalist - all", title.size = 2)

inat_june_fourfams <- inat_june_ortho %>%
  filter(!is.na(cats_4fam_effort), cats_4fam_effort > 0)

four_fams <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(inat_june_fourfams) + tm_polygons(col = "cats_4fam_effort", palette = "YlGnBu", title = "Occurrence", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, outer.margins = c(0.01,0,0.01,0), 
            inner.margins = c(0.02, 0.02, 0.1, 0.1), title = "D. iNaturalist - woody", title.size = 2)

cc_all_cats <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(cc_id_june_ortho) +
  tm_polygons(col = "n_cats", palette = "YlGnBu", title = "Density", alpha = 0.65, breaks = c(0, 0.04, 0.08, 0.12, 0.14)) + 
  tm_shape(cc_id_june_sites) + tm_dots(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, outer.margins = c(0.01,0,0.01,0),
            inner.margins = c(0.02, 0.02, 0.1, 0.1), title = "B. Caterpillars Count! - all", title.size = 2)

cc_four_fams <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(cc_id_june_ortho) + tm_polygons(col = "n_cats_4fam", palette = "YlGnBu", title = "Density", alpha = 0.65) + 
  tm_shape(cc_id_june_sites) + tm_symbols(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, outer.margins = c(0.01,0,0.01,0), 
            inner.margins = c(0.02, 0.02, 0.1, 0.1), title = "E. Caterpillars Count! - woody", title.size = 2)

cc_inat_11 <- inat_june %>%
  mutate_at(c("year"), ~as.numeric(.)) %>%
  right_join(cc_id_june, by = c("year", "cell"), suffix = c("_inat", "_cc"))

# Correlation tests for inat/cc comparisons
cor.test(cc_inat_11$cats_effort, cc_inat_11$n_cats_cc)
cor.test(cc_inat_11$cats_4fam_effort, cc_inat_11$n_cats_4fam_cc)

theme_set(theme_classic(base_size = 23))
inat_cc_11_plot <- ggplot(cc_inat_11, aes(x = cats_effort, y = n_cats_cc)) + geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, col = "darkgray", cex = 1.5) +
  annotate(geom = "text", x = 0.075, y = 0.02, label = c(expression(italic('r') == 0.73 )), size = 9) +
  labs(x = "iNaturalist occurrence", y = "Caterpillars Count! density", title = "C. All") +
  theme(plot.title = element_text(hjust = -0.25))

inat_cc_11_4fam_plot <- ggplot(cc_inat_11, aes(x = cats_4fam_effort, y = n_cats_4fam_cc)) + geom_point(cex = 3) + 
  labs(x = "iNaturalist occurrence", y = "Caterpillars Count! density", title = "F. Woody") +
  geom_smooth(method = "lm", se = F, col = "darkgray", cex = 1.5) +
  annotate(geom = "text", x = 0.035, y = 0.005, label = c(expression(italic('r') == 0.40 )), size = 9) +
  theme(plot.title = element_text(hjust = -0.3))


grid.newpage()
pdf(paste0(getwd(),"/InTheMiddle/figs/inat_cc_data_density_six_panel.pdf"), height = 10, width = 18)
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
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5)

cc_meandens <- tm_shape(easternNA_zoom) + tm_polygons() +
  tm_shape(cc_avgs_june_hex) + 
  tm_polygons(col = "meanMeanDensity", size = 0.25, palette = "YlGnBu", title = "Caterpillars/survey", 
              alpha = 0.65, breaks = breaks_meandens) + 
  tm_shape(catcount_sites_june_ortho) + tm_dots(col = "black", size = 0.05) +
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5)

cc_meanbiomass <- tm_shape(easternNA_zoom) + tm_polygons() +
  tm_shape(cc_avgs_june_hex) + 
  tm_polygons(col = "meanMeanBiomass", size = 0.25, palette = "YlGnBu", title = "Biomass (mg)/survey", 
              alpha = 0.65, breaks = breaks_meanbiomass) + 
  tm_shape(catcount_sites_june_ortho) + tm_dots(col = "black", size = 0.05) +
  tm_layout(legend.text.size = 1.25, legend.title.size = 1.5) 

cc_map <- tmap_arrange(cc_fracsurveys, cc_meandens, cc_meanbiomass, ncol = 3)
tmap_save(cc_map, "figs/caterpillars-count/catcount_june_map.pdf", units = "in", width = 18, height = 6)
