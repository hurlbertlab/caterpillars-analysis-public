# Examining caterpillar phenology with iNaturalist data

# iNaturalist data requested in October 2018

#### Libraries ####
library(tidyverse)
library(taxize)
library(lubridate)
library(maps)
library(rgdal)
library(sf)
library(tmap)
library(forcats)
library(dggridR)

#####  Read in data ########
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/inat_species.txt", header = T, sep = "\t")

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# hex grid and lat/lon coords of cell centers

hex_df <- dggridR::dgconstruct(res = 6)

cell_centers <- read.csv("data/hex_grid_cell_centers.csv")
cell_centers$cell <- as.factor(cell_centers$cell + 0.1)

hex <- st_read("data/maps/hex_grid.shp") %>%
  left_join(cell_centers, by = c("id" = "cell")) %>%
  filter(!is.na(lat))

# Criteria for inclusion (records refers to survey events)
minNumRecords = 40
minNumDates = 4

siteList = fullDataset %>%
  filter(Year == 2018) %>%
  group_by(Name, Region, Latitude, Longitude) %>%
  summarize(nRecs = n_distinct(ID),
            nDates = n_distinct(LocalDate)) %>%
  arrange(desc(Latitude)) %>%
  filter(nRecs >= minNumRecords, nDates >= minNumDates, Name != "Example Site") %>%
  mutate(county = latlong2county(data.frame(lon = Longitude, lat = Latitude)))

# Aggregate iNat data by day, year, and lat-lon bin
inat$observed_on = as.Date(inat$observed_on, format = "%Y-%m-%d")
inat$year = format(inat$observed_on, format = "%Y")
inat$jday = yday(inat$observed_on)
inat$jd_wk = 7*floor(inat$jday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

#### Filter data ####
## Subset iNat by CC sites
jdBeg = 91
jdEnd = 240

inat_noCC <- inat %>%
  filter(user_login != "caterpillarscount", year > 2014, jday >= jdBeg, jday <= jdEnd) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(cell = as.character(dgGEO_to_SEQNUM(hex_df, longitude, latitude)$seqnum + 0.1)) %>%
  group_by(year, cell, jd_wk) %>%
  summarize(n_inat = n())

cc_recent <- fullDataset %>%
  filter(Name %in% siteList$Name, Year > 2014) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(cell = as.character(dgGEO_to_SEQNUM(hex_df, Longitude, Latitude)$seqnum + 0.1),
         jd_wk = 7*floor(julianday/7) + 4) %>%
  filter(Group == "caterpillar") %>%
  group_by(cell, Year, jd_wk) %>%
  summarize(n_CC = sum(Quantity))

inat_cc_bins <- cc_recent %>%
  left_join(inat_noCC, by = c("cell", "Year" = "year", "jd_wk")) %>%
  group_by(Year, cell) %>%
  summarize(sum_CC = sum(n_CC, na.rm = T), sum_inat = sum(n_inat, na.rm = T))

##### Plot number of records ####
# Plot of records within radius of CC site
NA_sf <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

easternUS <- st_bbox(c(xmin = -100, xmax = -64, ymin = 25, ymax = 50), crs = st_crs(NA_sf))

US_map <- tm_shape(NA_sf, bbox = easternUS) + tm_borders(col = "grey80") + tm_fill(col = "grey90")

obs_sf <- hex %>%
  left_join(inat_cc_bins, by = c("id" = "cell")) %>%
  filter(!is.na(sum_inat), Year < 2019)


map <- US_map + tm_shape(obs_sf) + 
  tm_polygons(col = "sum_CC", 
             title = "Caterpillars Count caterpillars",
             palette = "GnBu", perceptual = T, 
             breaks = seq(1, 400, by = 50), scale = 4, alpha = 0.75) +
  tm_text(text = "sum_inat") +
  tm_facets(by = "Year", nrow = 2)
tmap_save(map, paste('figs/iNat_caterpillar_nearCC_phenomap_byYear_hex_jd_', jdBeg, '-', jdEnd, '.pdf', sep = ''),
          height = 6, width = 12, units = "in")


##### Plot iNat phenology ####
## Overlay iNat on red curves figure
# Average iNat observations by week

# Set up bins, date filters 

minNumRecords = 267
minNumDates = 6

sites_top <- siteList %>%
  filter(nRecs >= minNumRecords,
         nDates >= minNumDates,
         Name != "Example Site", Name != "East Carolina University", Name != "NC State University")

# Filter and merge data
# For each CC site from top ~12, get iNaturalist around that site 

cc_top <- fullDataset %>%
  filter(Name %in% sites_top$Name, Year == 2018) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(cell = as.character(dgGEO_to_SEQNUM(hex_df, Longitude, Latitude)$seqnum + 0.1),
         jd_wk = 7*floor(julianday/7) + 4) %>%
  filter(Year == 2018) %>%
  group_by(cell, jd_wk) %>%
  mutate(n = n()) %>%
  group_by(cell, jd_wk) %>%
  summarize(totalCount = sum(Quantity, na.rm = T),
            numSurveysGTzero = length(unique(ID[Quantity > 0])),
            fracSurveys = 100*numSurveysGTzero/mean(n))

sitedata <- cc_top %>%
  left_join(filter(inat_noCC, year == 2018), by = c("cell", "jd_wk")) %>%
  left_join(cell_centers) %>%
  mutate(cell_labs = paste0(round(lon, 2), " , ", round(lat, 2)))

# Plot
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())

phenoplot <- ggplot(sitedata, aes(x = jd_wk)) + 
  facet_wrap(~fct_reorder(cell_labs, lat, .desc = T), nrow = 3) +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(x = "", y = "Fraction of surveys with caterpillars") +
  scale_y_continuous(sec.axis = dup_axis(name = "Number of iNaturalist observations"))

phenoplot + 
  geom_line(aes(y = fracSurveys, col = "Caterpillars Count"), cex = 1) +
  geom_line(aes(y = n_inat, color = "iNaturalist"), cex = 1) +
  scale_color_manual(values = c("Caterpillars Count" = "firebrick", 
                                "iNaturalist" = "dodgerblue")) +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figs/CaterpillarPhenology_withiNat_2018.pdf", width = 12, height = 8, units = "in")


#### Determine/plot widespread families ####
## Common/widespread families from iNat

inat_families <- inat_noCC %>%
  left_join(inat_species, by = "scientific_name")

# Phenology of families (make abundance curves on map plot for each family)
family_pheno <- inat_families %>%
  filter(!is.na(family), family != "") %>%
  filter(year == 2018) %>%
  filter(latitude >= 25) %>%
  mutate(lat_band = ifelse(latitude < 35, "25-35° Latitude", 
                           ifelse(latitude < 45, "35-45° Latitude", "45-60° Latitude"))) %>%
  group_by(family, lat_band, jd_wk) %>%
  count()

# Plot: line plots for each family, separate lines for latitudinal regions
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())
ggplot(family_pheno) + 
  geom_point(aes(x = jd_wk, y = n, col = lat_band)) + 
  geom_line(aes(x = jd_wk, y = n, col = lat_band), cex = 1) +
  scale_y_log10() + facet_wrap(~family, nrow = 4) +
  scale_x_continuous(breaks = jds, labels = dates) +
  ylab("Number of observations") + theme(axis.title.x = element_blank()) +
  theme(legend.position = c(0.9, 0.1), legend.title = element_blank()) +
  scale_color_viridis_d()
ggsave("figs/CaterpillarPhenology_Families_2018.pdf", width = 12, height = 8, units = "in")

# For 10 most abundant families, stacked bar chart showing number observations/family each JD week
fam_abund <- family_pheno %>%
  group_by(family) %>%
  summarize(sum = sum(n)) %>%
  arrange(desc(sum)) %>%
  filter(sum > 100)
  
family_pheno_abund <- family_pheno %>%
  filter(family %in% fam_abund$family)

ggplot(family_pheno_abund) +
  geom_col(aes(x = jd_wk, y = n, fill = family), position = "stack") +
  scale_x_continuous(breaks = jds, labels = dates) +
  scale_fill_brewer(palette = "Set3") +
  ylab("Number of observations") + theme(axis.title.x = element_blank(), legend.title = element_blank())
ggsave("figs/CaterpillarPhenology_Families_2018_byWeek.pdf", width = 8, height = 6, units = "in")

# Plot geographic distribution of families
jdBeg = 91
jdEnd = 240
recsPerBinThreshold = 100

widespread_families <- family_pheno %>%
  group_by(family) %>%
  summarize(sum_recs = sum(n)) %>%
  filter(sum_recs > recsPerBinThreshold)

inat_families_spring <- inat_families %>%
  filter(!is.na(family), family != "") %>%
  filter(year == 2018) %>%
  filter(jday <= jdEnd, jday >= jdBeg) %>%
  filter(!is.na(longitude), !is.na(latitude))

NA_sf <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')
easternUS <- st_bbox(c(xmin = -100, xmax = -64, ymin = 25, ymax = 50), crs = st_crs(NA_sf))
US_map <- tm_shape(NA_sf, bbox = easternUS) + tm_borders(col = "grey80") + tm_fill(col = "grey90")

inat_fam_sf <- st_as_sf(inat_families_spring, coords = c("longitude", "latitude"))

fam_map <- US_map + tm_shape(inat_fam_sf) + tm_dots(size = 0.25, alpha = 0.75) + tm_facets(by = "family", nrow = 4) + 
  tm_layout(panel.label.size = 2)
tmap_save(fam_map, "figs/CaterpillarFamily_Range_iNat_2018.pdf", width = 12, height = 8, units = "in")

### Families in CC data
## 489 CC observations in iNaturalist, 113 ID-d to family

## Plot: what percent of caterpillars count observations are identified over time?
inat_CC_dates <- inat %>%
  filter(user_login == "caterpillarscount", jday >= jdBeg, jday <= jdEnd) %>%
  group_by(year, jd_wk) %>%
  mutate(nObs = n()) %>%
  filter(scientific_name != "Lepidoptera") %>%
  group_by(year, jd_wk, nObs) %>%
  summarize(n = n()) %>%
  mutate(pctID = n/nObs)

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_classic())
ggplot(inat_CC_dates, aes(x = jd_wk, y = pctID)) + geom_col() +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(x = "", y = "% Identified") + facet_grid(~year)
ggsave("figs/percent_CC_obs_identified_byweek.pdf", units = "in", height = 6, width = 14)

## Subset iNat by CC sites
jdBeg = 91
jdEnd = 240

inat_CC <- inat %>%
  filter(user_login == "caterpillarscount", jday >= jdBeg, jday <= jdEnd) %>%
  left_join(inat_species, by = "scientific_name")

# Phenology of families (make abundance curves on map plot for each family)
family_pheno_CC <- inat_CC %>%
  filter(!is.na(family), family != "") %>%
  filter(year == 2018) %>%
  filter(latitude >= 25) %>%
  mutate(lat_band = ifelse(latitude < 35, "25-35° Latitude", 
                           ifelse(latitude < 45, "35-45° Latitude", "45-60° Latitude"))) %>%
  group_by(family, lat_band, jd_wk) %>%
  count()

# Plot: line plots for each family, separate lines for latitudinal regions
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())
ggplot(family_pheno_CC) + 
  geom_point(aes(x = jd_wk, y = n, col = lat_band)) + 
  geom_line(aes(x = jd_wk, y = n, col = lat_band), cex = 1) +
  scale_y_log10() + facet_wrap(~family, nrow = 4) +
  scale_x_continuous(breaks = jds, labels = dates) +
  ylab("Number of observations") + theme(axis.title.x = element_blank()) +
  theme(legend.position = c(0.9, 0.1), legend.title = element_blank()) +
  scale_color_viridis_d()
ggsave("figs/CaterpillarPhenology_Families_CC_2018.pdf", width = 12, height = 8, units = "in")

# For 10 most abundant families, stacked bar chart showing number observations/family each JD week

ggplot(family_pheno_CC) +
  geom_col(aes(x = jd_wk, y = n, fill = family), position = "stack") +
  scale_x_continuous(breaks = jds, labels = dates) +
  scale_fill_brewer(palette = "Set3") +
  ylab("Number of observations") + theme(axis.title.x = element_blank(), legend.title = element_blank())
ggsave("figs/CaterpillarPhenology_Families_2018_byWeek_CC.pdf", width = 8, height = 6, units = "in")

# Plot geographic distribution of families
jdBeg = 91
jdEnd = 240
recsPerBinThreshold = 100

widespread_families <- family_pheno %>%
  group_by(family) %>%
  summarize(sum_recs = sum(n)) %>%
  filter(sum_recs > recsPerBinThreshold)

inat_families_CC <- inat_CC %>%
  filter(!is.na(family), family != "") %>%
  filter(jday <= jdEnd, jday >= jdBeg) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(latitude, longitude, family) %>%
  summarize(nObs = n())

NA_sf <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')
easternUS <- st_bbox(c(xmin = -100, xmax = -64, ymin = 25, ymax = 50), crs = st_crs(NA_sf))
US_map <- tm_shape(NA_sf, bbox = easternUS) + tm_borders(col = "grey80") + tm_fill(col = "grey90")

inat_fam_CC_sf <- st_as_sf(inat_families_CC, coords = c("longitude", "latitude"))

fam_map <- US_map + tm_shape(inat_fam_CC_sf) + tm_dots(size = "nObs", alpha = 0.5) + tm_facets(by = "family", nrow = 4) + 
  tm_layout(panel.label.size = 2)
tmap_save(fam_map, "figs/CaterpillarFamily_Range_iNat_2018_CC.pdf", width = 12, height = 8, units = "in")

####### Compare iNaturalist and Caterpillars Count data: only 4 most common CatCount families ######

## Plot: 4 families, phenology curves, iNat and CC sites
# Filter and merge data

inat_noCC_families <- inat %>%
  left_join(inat_species, by = "scientific_name") %>%
  filter(user_login != "caterpillarscount", year > 2014, jday >= jdBeg, jday <= jdEnd) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(family %in% c("Geometridae", "Erebidae", "Notodontidae", "Noctuidae")) %>%
  mutate(cell = as.character(dgGEO_to_SEQNUM(hex_df, longitude, latitude)$seqnum + 0.1)) %>%
  group_by(year, cell, jd_wk) %>%
  summarize(n_inat = n()) %>%
  filter(year == 2018)

family_surveys <- cc_top %>%
  left_join(inat_noCC_families, by = c("cell", "jd_wk")) %>%
  left_join(cell_centers) %>%
  mutate(cell_labs = paste0(round(lon, 2), " , ", round(lat, 2)))

# Plot
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())

phenoplot <- ggplot(family_surveys, aes(x = jd_wk)) + 
  facet_wrap(~fct_reorder(cell_labs, lat, .desc = T), nrow = 3) +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(x = "", y = "Fraction of surveys with caterpillars") +
  scale_y_continuous(sec.axis = dup_axis(name = "Number of iNaturalist observations"))

phenoplot + 
  geom_line(aes(y = fracSurveys, col = "Caterpillars Count"), cex = 1) +
  geom_line(aes(y = n_inat, color = "iNaturalist"), cex = 1) +
  scale_color_manual(values = c("Caterpillars Count" = "firebrick", 
                                "iNaturalist" = "dodgerblue")) +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figs/FourCatFamilies_Phenology_withiNat_2018.pdf", width = 12, height = 8, units = "in")
