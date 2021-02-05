## Get CC data for trait-based lep phenocurve validation

library(tidyverse)

### Read in CC data and functions
source('code/analysis_functions_gd.r')
source('code/reading_datafiles_without_users_gd.r')

### Site effort summary

site_effort <- data.frame(year = c(2000:2020)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDataset, year = y)
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
  group_by(Name, Region, cell, Latitude, Longitude, Year, julianweek) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(Quantity2[Group == "caterpillar"], na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0 & Group == "caterpillar"])),
            totalBiomass = sum(Biomass_mg[Group == "caterpillar"], na.rm = TRUE)) %>% 
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys)
write.csv(site_pheno, "data/derived_data/cc_subset_trait_based_pheno.csv", row.names = F)
