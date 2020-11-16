## Measuring deviations from average phenology metrics in Caterpillars Count and iNaturalist data

### Libraries
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
library(cowplot)

### Read in CC data and functions
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

### Read in iNat data and mapping files

inat = read.csv('data/inat_caterpillars_eastern_NA_5-20-2020.csv', header = TRUE, stringsAsFactors = F)

NAmap = read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/inat_caterpillar_species_traits.txt", header = T, sep = "\t")

hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")

### Site effort summary

site_effort <- data.frame(year = c(2015:2019)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDataset, year = y)
  })) %>%
  unnest(cols = c(site_effort))

## For nGoodWeeks 3-10, how many sites with at least 2 years in analysis?

years_good <- tibble(minWeeksGoodor50Surveys = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeksGoodor50Surveys, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeksGoodor50Surveys >= nweeks, modalSurveyBranches >= 20) %>%
      group_by(Name) %>%
      mutate(n_years = n_distinct(year)) %>%
      filter(n_years >= 2)
  }),
  n_siteyears = purrr::map_dbl(sites, ~nrow(.)),
  n_regions = purrr::map_dbl(sites, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(sites, ~length(unique(.$cell))))

goodweeks <- ggplot(years_good, aes(x = minWeeksGoodor50Surveys)) + 
  geom_line(aes(y = n_siteyears, col = "Site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = " ", x = "Minimum good weeks or weeks with 50 surveys") +
  theme(legend.position = c(0.8, 0.9))

## For nWeeks 3-10

yearpairs_all <- tibble(minWeeks = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeks, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeks >= nweeks, modalSurveyBranches >= 20) %>%
      group_by(Name) %>%
      mutate(n_years = n_distinct(year)) %>%
      filter(n_years >= 2)
  }),
  n_siteyears = purrr::map_dbl(sites, ~nrow(.)),
  n_regions = purrr::map_dbl(sites, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(sites, ~length(unique(.$cell))))

allweeks <- ggplot(yearpairs_all, aes(x = minWeeks)) + 
  geom_line(aes(y = n_siteyears, col = "Site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = "Data points", x = "Minimum weeks") +
  theme(legend.position = c(0.8, 0.9))

plot_grid(allweeks, goodweeks, ncol = 2)
# ggsave("figs/caterpillars-count/pheno_data_sites_per_year.pdf", units = "in", height = 5, width = 10)

## If at least 6 good weeks

focal_sites <- years_good %>%
  filter(minWeeksGoodor50Surveys == 6) %>%
  unnest(cols = c("sites"))

focal_years_plot <-  focal_sites %>% 
  pivot_longer(firstGoodDate:lastGoodDate, names_to = "time", values_to = "dates")

# plot panel for each site with at least 8 good weeks
# year vs. jday, lines from firstGoodDate to lastGoodDate

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(focal_years_plot) + geom_path(aes(x = dates, y = year, group = year), size = 2) + 
  scale_x_continuous(breaks = jds, labels = dates) + facet_wrap(~Name) +
  labs(x = " ", y = " ")
# ggsave("figs/caterpillars-count/pheno_siteyears_overlap.pdf", units = "in", height = 10, width = 15)

# For sites with at least 6 good weeks, find common time window across years for sites (w/ at least 6 weeks overlap)

surveyThreshold = 0.8            # proprortion of surveys conducted to be considered a good sampling day
minJulianWeek = 135              # beginning of seasonal window for tabulating # of good weeks
maxJulianWeek = 211

site_overlap <- fullDataset %>%
  right_join(focal_sites, by = c("Year" = "year", "Name", "Region", "cell", "Latitude", "Longitude", "medianGreenup")) %>%
  filter(case_when(Name == "UNC Chapel Hill Campus" ~ julianweek >= 121, # for UNC Campus, take out BIO 101 observations in April
                   TRUE ~ TRUE)) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, julianweek, medianGreenup) %>%
  mutate(nSurveysPerWeek = n_distinct(ID),
            nSurveyBranches = n_distinct(PlantFK)) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(good_week = ifelse((julianweek >= minJulianWeek & julianweek <= maxJulianWeek) & 
                                         (nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek | nSurveysPerWeek > 50), 1, 0)) %>%
  filter(good_week == 1) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(Start = min(julianweek),
         End = max(julianweek)) %>% 
  group_by(Name, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(maxStart = max(Start),
         minEnd = min(End)) %>%
  filter(julianweek >= maxStart & julianweek <= minEnd) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  filter(n_distinct(julianweek) >= 6) %>%
  group_by(Name, Region, cell, Latitude, Longitude, medianGreenup) %>%
  filter(n_distinct(Year) >= 2)

sites_6weeks_overlap <- site_overlap %>%
  ungroup() %>%
  distinct(Name, Year, Region, cell, Latitude, Longitude, medianGreenup, Start, End)

hex_start_end <- sites_6weeks_overlap %>%
  group_by(Year, cell) %>%
  summarize(start = min(Start),
            end = max(End)) %>%
  ungroup() %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

## Calculate pheno anomalies for sites with at least 2 years, min six weeks of good survey overlap between years
## Same start/end dates across years
## Anomalies in centroid, peak date

outlierCount = 10000

pheno_dev <- site_overlap %>%
  mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
  group_by(Name, Region, cell, Latitude, Longitude, Year, julianweek) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(Quantity2[Group == "caterpillar"], na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0 & Group == "caterpillar"])),
            totalBiomass = sum(Biomass_mg[Group == "caterpillar"], na.rm = TRUE)) %>% 
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys) %>%
  group_by(Name, Year, cell) %>%
  summarize(pctPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                 julianweek[fracSurveys == max(fracSurveys, na.rm = TRUE)][1]),
            pctCentroidDate = sum(julianweek*fracSurveys)/sum(fracSurveys)) %>%
  group_by(Name, cell) %>%
  mutate(meanPeakDate = mean(pctPeakDate),
         meanCentroidDate = mean(pctCentroidDate),
         devPeakDate = meanPeakDate - pctPeakDate,
         devCentroidDate = meanCentroidDate - pctCentroidDate) 

cell_pheno <- pheno_dev %>%
  group_by(cell, Year) %>%
  summarize(avgDevPeak = mean(devPeakDate),
            avgDevCentroid = mean(devCentroidDate))

hex_subset <- hex %>%
  filter(cell %in% cell_pheno$cell)

## Pull weekly iNaturalist cats excluding caterpillars_count for min start and max end dates of cell/years from cell_pheno
## Pull weekly iNaturalist arthropod observations for the same set of weeks to do effort correction
## At least 100 insect observations per hex cell (2017-2019)

# Pull iNaturalist insect data (to 2018)

# setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")
# 
# db_list_tables(con)
# 
# inat_insects_early_db <- tbl(con, "inat") %>%
#   dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
#   filter(!is.na(longitude) | !is.na(latitude)) %>%
#   filter(latitude > 15, latitude < 90, longitude > -100, longitude < -30) %>%
#   filter(iconic_taxon_name == "Insecta") %>%
#   mutate(year = substr(observed_on, 1, 4),
#          month = substr(observed_on, 6, 7)) 
# 
# inat_insects_early_df <- inat_insects_early_db %>%
#   collect()
# 
# inat_insects_subset <- inat_insects_early_df %>%
#   filter(year >= 2015, year <= 2018) %>%
#   mutate(jday = yday(observed_on), 
#          jd_wk = 7*floor(jday/7)) %>%
#   filter(jd_wk >= minJulianWeek, jd_wk <= maxJulianWeek)
# 
# inat_insects_cells <- inat_insects_subset %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_set_crs(4326) %>%
#   st_intersection(hex_subset)
# 
# inat_insects_weekly_cells <- inat_insects_cells %>%
#   st_set_geometry(NULL) %>%
#   group_by(cell, year, jd_wk) %>%
#   summarize(nObs = n_distinct(id))
# write.csv(inat_insects_weekly_cells, "data/inat_2015-2018_weekly_insecta_hex.csv", row.names = F)

inat_insects_weekly_cells <- read.csv("data/inat_2015-2018_weekly_insecta_hex.csv", stringsAsFactors = F)

## iNaturalist weekly Insecta observations (2019)

# inat_june_2019 <- read.csv("inat_june_2019/inat_2019_06.csv", stringsAsFactors = F)
# inat_june_insecta <- inat_june_2019 %>%
#   filter(iconic_taxon_name == "Insecta") %>%
#   mutate(year = substr(observed_on, 1, 4),
#          jday = yday(observed_on), 
#          jd_wk = 7*floor(jday/7)) %>%
#   filter(jd_wk >= minJulianWeek, jd_wk <= maxJulianWeek) %>%
#   dplyr::select(id, observed_on, user_login, latitude, longitude, year, jday, jd_wk) %>%
#   mutate_at(c("id", "latitude"), as.numeric)
# 
# summer_files <- data.frame(filename = list.files("inat_insecta_summer_2019")) %>%
#   filter(grepl("observations", filename)) %>%
#   group_by(filename) %>%
#   nest() %>%
#   mutate(file = purrr::map(filename, ~read.csv(paste0("inat_insecta_summer_2019/", .), stringsAsFactors = F)))
#   
# summer_table <- summer_files %>%
#   dplyr::select(-data) %>%
#   unnest(cols = c("file")) %>%
#   mutate(year = substr(observed_on, 1, 4),
#          jday = yday(observed_on), 
#          jd_wk = 7*floor(jday/7)) %>%
#   filter(jd_wk >= minJulianWeek, jd_wk <= maxJulianWeek) %>%
#   ungroup() %>%
#   dplyr::select(id, observed_on, user_login, latitude, longitude, year, jday, jd_wk)
# 
# inat_summer_2019 <- bind_rows(inat_june_insecta, summer_table) %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_set_crs(4326) %>%
#   st_intersection(hex) %>%
#   st_set_geometry(NULL) %>%
#   group_by(cell, year, jd_wk) %>%
#   summarize(nObs = n_distinct(id))
# write.csv(inat_summer_2019, "data/inat_2019_weekly_insecta_hex.csv", row.names = F)

inat_summer_2019 <- read.csv("data/inat_2019_weekly_insecta_hex.csv", stringsAsFactors = F)

# Where can we calculate iNat deviations

inat_insect_allyrs <- inat_summer_2019 %>%
  filter(cell %in% hex_subset$cell) %>%
  bind_rows(inat_insects_weekly_cells) %>%
  filter(nObs > 50) %>%
  ungroup() %>%
  mutate_at(c("year", "cell"), as.numeric) %>%
  left_join(hex_start_end, by = c("year" = "Year", "cell")) %>%
  filter(jd_wk <= end, jd_wk >= start) %>%
  group_by(cell, year) %>%
  mutate(n_wks = n()) %>%
  filter(n_wks >= 6)

# Calculate caterpillars/insect observations per week for inaturalist

inat_cats_pheno <- inat %>%
  filter(user_login != "caterpillarscount") %>%
  mutate(year = as.numeric(substr(observed_on, 1, 4)),
         jday = yday(observed_on), 
         jd_wk = 7*floor(jday/7)) %>%
  filter(year >= 2015, year <= 2019) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_intersection(hex) %>%
  st_set_geometry(NULL) %>%
  group_by(cell, year, jd_wk) %>%
  summarize(nCats = n_distinct(id))

## Peak and centroid dates and anomalies for caterpillars (effort corrected)

inat_pheno <- inat_cats_pheno %>%
  right_join(inat_insect_allyrs, by = c("cell", "year", "jd_wk")) %>%
  mutate(cat_effort = nCats/nObs) %>%
  group_by(cell, year) %>%
  summarize(peakDate = median(jd_wk[cat_effort == max(cat_effort, na.rm = T)], na.rm = T),
            centroidDate = sum(jd_wk*cat_effort, na.rm = T)/sum(cat_effort, na.rm = T)) %>%
  group_by(cell) %>%
  mutate(meanPeakDate = mean(peakDate),
         meanCentroidDate = mean(centroidDate),
         devPeakDate = meanPeakDate - peakDate,
         devCentroidDate = meanCentroidDate - centroidDate)

## Temperature anomalies

hex_temps <- read.csv("data/hex_mean_temps.csv", stringsAsFactors = F)

cell_pheno <- cell_pheno %>%
  ungroup() %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

temp_dev <- hex_temps %>%
  right_join(inat_pheno, by = c("cell", "year")) %>%
  group_by(cell) %>%
  mutate(tempDev = mean(mean_temp) - mean_temp) %>%
  left_join(cell_pheno, by = c("cell", "year" = "Year"))

## Figure: peak and centroid date deviations vs temperature deviations, peak & centroid v temp and lat in 2019 iNat and CC diff colors

theme_set(theme_classic(base_size = 17))

# read in 2019 peak, centroid, temp, lat data
pheno_plot <- read.csv("InTheMiddle/data/pheno_2019_cc_inat_plot.csv", stringsAsFactors = F)

peak_dev <- temp_dev %>%
  dplyr::select(cell, year, mean_temp, tempDev, devPeakDate, avgDevPeak) %>%
  pivot_longer(devPeakDate:avgDevPeak, names_to = "metric", values_to = "deviance") %>%
  mutate(dataset = case_when(metric == "devPeakDate" ~ "iNaturalist",
                             TRUE ~ "Caterpillars Count!"))

# iNat peak model
summary(lm(deviance ~ tempDev, data = filter(peak_dev, dataset == "iNaturalist")))

# CC peak model
summary(lm(deviance ~ tempDev, data = filter(peak_dev, dataset == "Caterpillars Count!")))

CC_dev_plot <- ggplot(peak_dev, aes(x = tempDev, y = deviance, col = dataset)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, cex = 1) +
  scale_color_manual(values = c("blue3", "palegreen3")) +
  labs(x = "Spring temperature deviation (°C)", y = "Peak date deviation (days)", col = "", title = "C. Peak date deviation") +
  theme(legend.position = c(0.8, 0.9), plot.title = element_text(hjust = -0.25))


centroid_dev <- temp_dev %>%
  dplyr::select(cell, year, mean_temp, tempDev, devCentroidDate, avgDevCentroid) %>%
  pivot_longer(devCentroidDate:avgDevCentroid, names_to = "metric", values_to = "deviance") %>%
  mutate(dataset = case_when(metric == "devCentroidDate" ~ "iNaturalist",
                             TRUE ~ "Caterpillars Count!"))
  
# iNat centroid model
summary(lm(deviance ~ tempDev, data = filter(centroid_dev, dataset == "iNaturalist")))

# CC centroid model
summary(lm(deviance ~ tempDev, data = filter(centroid_dev, dataset == "Caterpillars Count!")))


CC_centroid_plot <- ggplot(centroid_dev, aes(x = tempDev, y = deviance, col = dataset)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, cex = 1) +
  scale_color_manual(values = c("blue3", "palegreen3")) +
  labs(x = "Spring temperature deviation (°C)", y = "Centroid date deviation (days)", col = "", title = "D. Centroid date deviation") +
  theme(legend.position = c(0.8, 0.9), plot.title = element_text(hjust = -0.25))

# Plot comparing cc and inaturalist deviations in peak date 
cc_inat_cent <- ggplot(temp_dev, aes(x = devCentroidDate, y = avgDevCentroid)) + 
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "B. Centroid date deviation") +
  theme(plot.title = element_text(hjust = -0.25))

cor.test(temp_dev$devCentroidDate, temp_dev$avgDevCentroid) # r = -0.354, p = 0.235

# Plot comparing cc and inatuarlist deviations in centroid date
cc_inat_peak <- ggplot(temp_dev, aes(x = devPeakDate, y = avgDevPeak)) + 
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "A. Peak date deviation") +
  theme(plot.title = element_text(hjust = -0.25))

cor.test(temp_dev$devPeakDate, temp_dev$avgDevPeak) # r = -0.217, p = 0.477

plot_grid(cc_inat_peak, cc_inat_cent,
  CC_dev_plot, CC_centroid_plot, ncol = 2)
ggsave("InTheMiddle/figs/phenometric_deviations.pdf", units = 'in', height = 10, width = 12)


#2019 pheno metrics
cols <- c("Caterpillars Count!" = "skyblue2", "iNaturalist" = "springgreen3")
line_types <- c("Peak date" = 1, "Centroid date" = 3)
shapes <- c("Peak date" = 15, "Centroid date" = 17)

lat_plot <- ggplot(pheno_plot, aes(x = lat)) + 
  geom_point(aes(y = peakDate, col = "iNaturalist", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = peakDate, col = "iNaturalist", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = avgPeakDate, col = "Caterpillars Count!", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = avgPeakDate, col = "Caterpillars Count!", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = centroidDate, col = "iNaturalist", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = centroidDate, col = "iNaturalist", lty = "Centroid date"), method = "lm", se = F) +
  geom_point(aes(y = avgCentroidDate, col = "Caterpillars Count!", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = avgCentroidDate, col = "Caterpillars Count!", lty = "Centroid date"), method = "lm", se = F) +
  scale_color_manual(name = "Dataset", values =  cols) +
  scale_shape_manual(name = "Pheno metric", values = shapes) +
  scale_linetype_manual(name = "Pheno metric", values = line_types) +
  labs(x = "Latitude", y = "Julian day")

# Scatterplot showing peak dates (col = dataset, symbol = metric) vs mean temperature

temp_plot <- ggplot(pheno_plot, aes(x = mean_temp)) + 
  geom_point(aes(y = peakDate, col = "iNaturalist", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = peakDate, col = "iNaturalist", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = avgPeakDate, col = "Caterpillars Count!", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = avgPeakDate, col = "Caterpillars Count!", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = centroidDate, col = "iNaturalist", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = centroidDate, col = "iNaturalist", lty = "Centroid date"), method = "lm", se = F) +
  geom_point(aes(y = avgCentroidDate, col = "Caterpillars Count!", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = avgCentroidDate, col = "Caterpillars Count!", lty = "Centroid date"), method = "lm", se = F) +
  scale_color_manual(name = "Dataset", values =  cols) +
  scale_shape_manual(name = "Pheno metric", values = shapes) +
  scale_linetype_manual(name = "Pheno metric", values = line_types) +
  theme(legend.position = "none") +
  labs(x = "Spring temperature (C)", y = "Julian day")

# Linear models of inat/cc centroid dates with spring temperature
# iNat centroid date
summary(lm(centroidDate ~ mean_temp, data = pheno_plot))

# CC centroid date
summary(lm(avgCentroidDate ~ mean_temp, data = pheno_plot))
