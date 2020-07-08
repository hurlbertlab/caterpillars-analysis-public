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

theme_set(theme_classic(base_size = 15))

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
ggsave("figs/caterpillars-count/pheno_data_sites_per_year.pdf", units = "in", height = 5, width = 10)

## If at least 8 good weeks

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
ggsave("figs/caterpillars-count/pheno_siteyears_overlap.pdf", units = "in", height = 10, width = 15)

# For sites with at least 6 good weeks, find common time window across years for sites (w/ at least 6 weeks overlap)

surveyThreshold = 0.8            # proprortion of surveys conducted to be considered a good sampling day
minJulianWeek = 102              # beginning of seasonal window for tabulating # of good weeks
maxJulianWeek = 214

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

## Pull weekly iNaturalist cats excluding caterpillars_count for min start and max end dates of cell/years from cell_pheno
## Pull weekly iNaturalist arthropod observations for the same set of weeks to do effort correction
## At least 100 insect observations per hex cell (2017-2019)

# Pull iNaturalist insect data (to 2018)

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
  filter(year >= 2017, year <= 2018) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  filter(jd_wk >= minJulianWeek, jd_wk <= maxJulianWeek)

inat_insects_early_df <- inat_insects_early_db %>%
  collect()

# iNaturalist
# Create weekly insect observations by hex cell for the inclusive weeks per hex cell from site_overlap

## Peak and centroid dates and anomalies for caterpillars (effort corrected)

## Figure: peak and centroid date deviations vs green up deviations, iNat and CC diff colors
