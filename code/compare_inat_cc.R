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

#####  Read in data ########
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/inat_species.txt", header = T, sep = "\t")

source('code/analysis_functions.r')
source('code/CCrawdata2masterdataframe.r')

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

inat$lat_round = round(inat$latitude, 1)
inat$lon_round = round(inat$longitude, 1)

#### Functions ####

# Count number of records per year and lat-long bin as a function of bin size (in degrees)
recsByBinYear = function(df, binsize) {
  df$lat_bin = binsize*floor(df$latitude/binsize) + binsize/2
  df$lon_bin = binsize*floor(df$longitude/binsize) + binsize/2
  
  inat_by_latlon_yr = df %>%
    group_by(lat_bin, lon_bin, year) %>%
    count()
  
  return(inat_by_latlon_yr)
}

recsByBinYearCC = function(df, binsize) {
  df$lat_bin = binsize*floor(df$Latitude/binsize) + binsize/2
  df$lon_bin = binsize*floor(df$Longitude/binsize) + binsize/2
  
  inat_by_latlon_yr = df %>%
    group_by(lat_bin, lon_bin, Year) %>%
    count()
  
  return(inat_by_latlon_yr)
}

#### Filter data ####
## Subset iNat by CC sites
jdBeg = 91
jdEnd = 240

inat_noCC <- inat %>%
  filter(user_login != "caterpillarscount", year > 2014, jday >= jdBeg, jday <= jdEnd)

one_deg <- recsByBinYear(inat_noCC, 1)
half_deg <- recsByBinYear(inat_noCC, 0.5)
quarter_deg <- recsByBinYear(inat_noCC, 0.25)

inat_bins <- bind_rows(data.frame(one_deg, bin = 1),
                       data.frame(half_deg, bin = 0.5),
                       data.frame(quarter_deg, bin = 0.25))

cc_recent <- fullDataset %>%
  filter(Name %in% siteList$Name, Year > 2014)

one_degCC <- recsByBinYearCC(cc_recent, 1)
half_degCC <- recsByBinYearCC(cc_recent, 0.5)
quarter_degCC <- recsByBinYearCC(cc_recent, 0.25)

cc_bins <- bind_rows(data.frame(one_degCC, bin = 1),
                     data.frame(half_degCC, bin = 0.5),
                     data.frame(quarter_degCC, bin = 0.25))

inat_cc_bins <- cc_bins %>%
  left_join(inat_bins, by = c("bin", "lat_bin", "lon_bin", "Year" = "year"), 
            suffix = c("_CC", "_inat")) %>%
  group_by(Year, bin, lat_bin, lon_bin) %>%
  summarize(sum_CC = sum(n_CC), sum_inat = sum(n_inat))

##### Plot number of records ####
# Plot of records within radius of CC site
NA_sf <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

easternUS <- st_bbox(c(xmin = -100, xmax = -64, ymin = 25, ymax = 50), crs = st_crs(NA_sf))

US_map <- tm_shape(NA_sf, bbox = easternUS) + tm_borders(col = "grey80") + tm_fill(col = "grey90")

obs_sf <- st_as_sf(inat_cc_bins, coords = c("lon_bin", "lat_bin"))
for(binsize in c(0.25, 0.5, 1)){
bins_sf <- obs_sf %>%
    filter(bin == binsize)
map <- US_map + tm_shape(bins_sf) + 
  tm_bubbles(size = "sum_CC", col = "sum_inat", 
             title.size = "Caterpillars Count surveys", title.col = "iNaturalist obs", 
             palette = "GnBu", perceptual = T, 
             breaks = seq(1, 400, by = 50), scale = 4, alpha = 0.75) +
  tm_facets(by = "Year", nrow = 2) + tm_layout(title = paste0(binsize, "° bin size"))
tmap_save(map, paste('figs/iNat_caterpillar_nearCC_phenomap_byYear_', binsize, 'deg_bin_jd_', jdBeg, '-', jdEnd, '.pdf', sep = ''),
          height = 8, width = 12, units = "in")
}

##### Plot iNat phenology ####
## Overlay iNat on red curves figure
# Average iNat observations by week

# Set up bins, date filters 

inat_noCC$lat_bin <- 1*floor(inat_noCC$latitude/1) + 1/2
inat_noCC$lon_bin <- 1*floor(inat_noCC$longitude/1) + 1/2
inat_noCC$bin_coord <- paste0(inat_noCC$lon_bin, ", ", inat_noCC$lat_bin)

siteList$lat_bin <- 1*floor(siteList$Latitude/1) + 1/2
siteList$lon_bin <- 1*floor(siteList$Longitude/1) + 1/2
siteList$bin_coord <- paste0(siteList$lon_bin, ", ", siteList$lat_bin)

fullDataset$lat_bin <- 1*floor(fullDataset$Latitude/1) + 1/2
fullDataset$lon_bin <- 1*floor(fullDataset$Longitude/1) + 1/2
fullDataset$bin_coord <- paste0(fullDataset$lon_bin, ", ", fullDataset$lat_bin)
fullDataset$jd_wk = 7*floor(fullDataset$julianday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

minNumRecords = 267
minNumDates = 4

# Filter and merge data

siteList = fullDataset %>%
  filter(Year == 2018) %>%
  group_by(Name, Region, Latitude, Longitude) %>%
  summarize(nRecs = n_distinct(ID),
            nDates = n_distinct(LocalDate)) %>%
  arrange(desc(Latitude)) %>%
  filter(nRecs >= minNumRecords, nDates >= minNumDates, Name != "Example Site", Name != "East Carolina University", Name != "NC State University") %>%
  mutate(county = latlong2county(data.frame(lon = Longitude, lat = Latitude)))

inat_surveys_plot <- inat_noCC %>%
  filter(year == 2018, bin_coord %in% siteList$bin_coord) %>%
  group_by(lat_bin, bin_coord, jd_wk) %>%
  count() %>%
  rename(N_obs_inat = n)

firstFilter = fullDataset %>%
  filter(julianday >= jdBeg, julianday <= jdEnd, Name %in% siteList$Name)

sitedata = firstFilter %>%
  filter(Year == 2018, Group == "caterpillar") %>%
  group_by(Latitude, bin_coord, Name, jd_wk) %>%
  summarize(totalCount = sum(Quantity, na.rm = T),
            numSurveysGTzero = length(unique(ID[Quantity > 0]))) %>%
  left_join(inat_surveys_plot, by = c("bin_coord", "jd_wk"))

# Plot
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())
ggplot(sitedata) + 
  geom_point(aes(x = jd_wk, y = totalCount), col = "firebrick") + geom_line(aes(x = jd_wk, y = totalCount), col = "firebrick", cex = 1) +
  geom_point(aes(x = jd_wk, y = N_obs_inat), col = "dodgerblue") + geom_line(aes(x = jd_wk, y = N_obs_inat), col = "dodgerblue", cex = 1) +
  scale_y_log10() + geom_text(aes(x = 130, y = 300, label = bin_coord)) + facet_wrap(~fct_reorder(Name, Latitude, .desc = T), nrow = 3) +
  scale_x_continuous(breaks = jds, labels = dates)
ggsave("figs/CaterpillarPhenology_withiNat_2018.pdf", width = 12, height = 8, units = "in")

### Different bin sizes, only MA, DC, NC sites

bins <- c(0.5, 1, 2)

for(bin in bins) {
binsize <- bin

inat_noCC$lat_bin <- binsize*floor(inat_noCC$latitude/binsize) + binsize/2
inat_noCC$lon_bin <- binsize*floor(inat_noCC$longitude/binsize) + binsize/2
inat_noCC$bin_coord <- paste0(inat_noCC$lon_bin, ", ", inat_noCC$lat_bin)

siteList$lat_bin <- binsize*floor(siteList$Latitude/binsize) + binsize/2
siteList$lon_bin <- binsize*floor(siteList$Longitude/binsize) + binsize/2
siteList$bin_coord <- paste0(siteList$lon_bin, ", ", siteList$lat_bin)

fullDataset$lat_bin <- binsize*floor(fullDataset$Latitude/binsize) + binsize/2
fullDataset$lon_bin <- binsize*floor(fullDataset$Longitude/binsize) + binsize/2
fullDataset$bin_coord <- paste0(fullDataset$lon_bin, ", ", fullDataset$lat_bin)
fullDataset$jd_wk = 7*floor(fullDataset$julianday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

minNumRecords = 267
minNumDates = 4

# Filter and merge data

sites <- siteList %>%
  filter(Region %in% c("MA", "DC", "NC"), Name != "UNC Chapel Hill Campus", Name != "East Carolina University",
         Name != "Currituck Banks Reserve")

inat_surveys_plot <- inat_noCC %>%
  filter(bin_coord %in% sites$bin_coord) %>%
  group_by(year, lat_bin, bin_coord, jd_wk) %>%
  count() %>%
  rename(N_obs_inat = n)

firstFilter = fullDataset %>%
  filter(julianday >= jdBeg, julianday <= jdEnd, Name %in% sites$Name)

effortByWeek <- firstFilter %>%
  group_by(Year, Region, bin_coord) %>%
  distinct(Year, ID, jd_wk) %>%
  count(jd_wk)

arthCount <- firstFilter %>%
  filter(Quantity < 10000, 
         Group %in% 'caterpillar') %>%
  group_by(Year, Region, bin_coord, jd_wk) %>%
  summarize(totalCount = sum(Quantity, na.rm = T),
            numSurveysGTzero = length(unique(ID[Quantity > 0]))) %>% 
  right_join(effortByWeek, by = c("Region", "bin_coord", "Year", "jd_wk")) %>%
  #next line replaces 3 fields with 0 if the totalCount is NA
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0) %>%
  mutate(meanDensity = totalCount/n,
         fracSurveys = 100*numSurveysGTzero/n) %>%
  right_join(filter(inat_surveys_plot, bin_coord %in% sites$bin_coord), by = c("bin_coord", "Year"="year", "jd_wk"))

# Plot
jds <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

theme_set(theme_bw())
phenoplot <- ggplot(arthCount, aes(x = jd_wk)) + 
  facet_wrap(fct_reorder(bin_coord, lat_bin, .desc = T)~Year, ncol = 4) +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(x = "", y = "Fraction of surveys with caterpillars") +
  scale_y_continuous(sec.axis = dup_axis(name = "Number of iNaturalist observations"))

phenoplot + geom_point(aes(y = fracSurveys, col = "Caterpillars Count")) + 
  geom_line(aes(y = fracSurveys, col = "Caterpillars Count"), cex = 1) +
  geom_point(aes(y = N_obs_inat, color = "iNaturalist")) + 
  geom_line(aes(y = N_obs_inat, color = "iNaturalist"), cex = 1) +
  scale_color_manual(values = c("Caterpillars Count" = "firebrick", 
                                    "iNaturalist" = "dodgerblue")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  geom_text(aes(x = 100, y = max(arthCount$N_obs_inat, na.rm = T), label = Region)) +
  ggtitle(paste0(binsize, "° bin size"))
  
ggsave(paste0("figs/CaterpillarPhenology_withiNat_2018_", binsize, "_deg.pdf"), width = 10, height = 10, units = "in")
}

#### Correlate NC CC and iNat observations, 1 deg bins, 2016-2018

binsize <- 1

inat_noCC$lat_bin <- binsize*floor(inat_noCC$latitude/binsize) + binsize/2
inat_noCC$lon_bin <- binsize*floor(inat_noCC$longitude/binsize) + binsize/2
inat_noCC$bin_coord <- paste0(inat_noCC$lon_bin, ", ", inat_noCC$lat_bin)

siteList$lat_bin <- binsize*floor(siteList$Latitude/binsize) + binsize/2
siteList$lon_bin <- binsize*floor(siteList$Longitude/binsize) + binsize/2
siteList$bin_coord <- paste0(siteList$lon_bin, ", ", siteList$lat_bin)

fullDataset$lat_bin <- binsize*floor(fullDataset$Latitude/binsize) + binsize/2
fullDataset$lon_bin <- binsize*floor(fullDataset$Longitude/binsize) + binsize/2
fullDataset$bin_coord <- paste0(fullDataset$lon_bin, ", ", fullDataset$lat_bin)
fullDataset$jd_wk <- 7*floor(fullDataset$julianday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

minNumRecords <- 267
minNumDates <- 4

# Filter and merge data

sites <- siteList %>%
  filter(Region %in% c("MA", "DC", "NC"), Name != "UNC Chapel Hill Campus", Name != "East Carolina University",
         Name != "Currituck Banks Reserve")

region_names <- unique(sites[, c(2, 10)])

inat_surveys_plot <- inat_noCC %>%
  filter(bin_coord %in% sites$bin_coord) %>%
  group_by(year, lat_bin, bin_coord, jd_wk) %>%
  count() %>%
  rename(N_obs_inat = n)

firstFilter <- fullDataset %>%
  filter(julianday >= jdBeg, julianday <= jdEnd, Name %in% sites$Name)

effortByWeek <- firstFilter %>%
  group_by(Year, Region, bin_coord) %>%
  distinct(Year, ID, jd_wk) %>%
  count(jd_wk)

arthCount_deltas <- firstFilter %>%
  filter(Quantity < 10000, 
         Group %in% 'caterpillar') %>%
  group_by(Year, Region, bin_coord, jd_wk) %>%
  summarize(totalCount = sum(Quantity, na.rm = T),
            numSurveysGTzero = length(unique(ID[Quantity > 0]))) %>% 
  right_join(effortByWeek, by = c("Region", "bin_coord", "Year", "jd_wk")) %>%
  #next line replaces 3 fields with 0 if the totalCount is NA
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0) %>%
  mutate(meanDensity = totalCount/n,
         fracSurveys = 100*numSurveysGTzero/n) %>%
  right_join(filter(inat_surveys_plot, bin_coord %in% sites$bin_coord), by = c("bin_coord", "Year"="year", "jd_wk")) %>%
  group_by(Year, bin_coord) %>%
  nest() %>%
  mutate(deltasDF = purrr::map(data, ~{
    df <- .
    len <- nrow(df) - 1
    deltaJD <- c()
    deltaFS <- c()
    deltaiNat <- c()
    for(i in 1:len) {
      JD <- df[i + 1, 2]$jd_wk - df[i, 2]$jd_wk
      FS <- df[i + 1, 7]$fracSurveys - df[i, 7]$fracSurveys
      iNat <- df[i + 1, 9]$N_obs_inat - df[i, 9]$N_obs_inat
      
      deltaJD <- c(deltaJD, JD)
      deltaFS <- c(deltaFS, FS)
      deltaiNat <- c(deltaiNat, iNat)
    }
    data.frame(deltaJD = deltaJD, deltaFS = deltaFS, deltaiNat = deltaiNat)
  })) %>%
  dplyr::select(Year, bin_coord, deltasDF) %>%
  unnest() %>%
  mutate(deltaFSwk = deltaFS/(deltaJD/7),
         deltaiNatwk = deltaiNat/(deltaJD/7)) %>%
  left_join(region_names, by = "bin_coord")

# Plot

theme_set(theme_bw())
ggplot(arthCount_deltas, aes(x = deltaFSwk, y = deltaiNatwk)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + 
  facet_wrap(~bin_coord) +
  geom_text(aes(x = -20, y = max(arthCount_deltas$deltaiNatwk, na.rm = T), label = Region)) +
  labs(x = "Change in iNaturalist observations by week", y = "Change in fraction of Cat Count surveys by week")
ggsave("figs/iNat_CC_correlations.pdf", height = 6, width = 8, units = "in")

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

