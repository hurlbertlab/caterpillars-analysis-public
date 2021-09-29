## Get CC data for trait-based lep phenocurve validation

library(tidyverse)

### Read in CC data and functions
source('code/analysis_functions.r')

datafiles = list.files('data/')[grepl("fullDataset", list.files('data/'))]
fullDataset = read.csv(paste('data/', datafiles[length(datafiles)], sep = ''), header = T)

## Read in file defining grouped sites:

# Note that it may not be valid to group adjacent sites together in every year.
#   --See code/grouping_sites_by_effort.r to visualize how effort compares between years
#   --Read in this file which describes the years for which the sites listed have comparable sampling effort.

groupedSites <- read.csv('data/grouped_CC_sites.csv', header = T)

# Calculate mean lats, longs, and greenup dates for each set of grouped sites
groupedSiteLatLongGreen <- fullDataset %>%
  distinct(Name, Latitude, Longitude, medianGreenup) %>%
  left_join(unique(groupedSites[, c('Name', 'GroupedName')]), by = 'Name') %>%
  group_by(GroupedName) %>%
  summarize(Latitude = mean(Latitude),
            Longitude = mean(Longitude),
            medianGreenup = mean(medianGreenup)) %>%
  filter(!is.na(GroupedName))

# Join grouped names, lats, longs, and greenup to fullDataset; straighten out column names              
fullDatasetGrouped <- fullDataset %>%
  left_join(groupedSites, by = c('Name', 'Year')) %>%
  left_join(groupedSiteLatLongGreen, by = 'GroupedName') %>%
  mutate(OriginalName = Name,
         Name = ifelse(!is.na(GroupedName), GroupedName, Name),
         Latitude = ifelse(!is.na(GroupedName), Latitude.y, Latitude.x),
         Longitude = ifelse(!is.na(GroupedName), Longitude.y, Longitude.x),
         medianGreenup = ifelse(!is.na(GroupedName), medianGreenup.y, medianGreenup.x))
                           

### Site effort summary

site_effort <- data.frame(year = c(2000:2020)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDatasetGrouped, year = y)
  })) %>%
  unnest(cols = c(site_effort))


### Sites with at least 6 good weeks

years_good <- tibble(minWeeksGoodor50Surveys = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeksGoodor50Surveys, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeksGoodor50Surveys >= nweeks, modalSurveyBranches >= 20) 
  }),
  n_siteyears = purrr::map_dbl(sites, ~nrow(.)),
  n_regions = purrr::map_dbl(sites, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(sites, ~length(unique(.$cell))))

focal_sites <- years_good %>%
  filter(minWeeksGoodor50Surveys == 6) %>%
  unnest(cols = c("sites"))

### Pull fullDataset for those sites

surveyThreshold = 0.8            # proprortion of surveys conducted to be considered a good sampling day
minJulianWeek = 135              # beginning of seasonal window for tabulating # of good weeks
maxJulianWeek = 211

site_data <- fullDataset %>%
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
  group_by(Name, Region, cell, Latitude, Longitude, medianGreenup) 

### Phenology at caterpillars count sites

outlierCount = 10000

site_pheno <- site_data %>%
  mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
  mutate(catCount = ifelse(Quantity2 > 1, 1, Quantity2)) %>% # For use with presence only data, count just one cat per branch
  group_by(Name, Region, cell, Latitude, Longitude, Year, julianweek) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(catCount[Group == "caterpillar"], na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0 & Group == "caterpillar"])),
            totalBiomass = sum(Biomass_mg[Group == "caterpillar"], na.rm = TRUE)) %>% 
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys)
write.csv(site_pheno, "data/derived_data/cc_subset_trait_based_pheno.csv", row.names = F)
