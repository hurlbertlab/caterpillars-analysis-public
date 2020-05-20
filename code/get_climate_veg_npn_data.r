# This file calculates climate and vegetation related summaries for Caterpillars Count! sites

library(dplyr)
library(lubridate)
library(raster)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')


################################################################
# Daymet

daymet = read.csv('data/env/daymet_climate.csv', header = T)

daymetGDD = climatedata %>%
  group_by(Name, year) %>%
  summarize(daymetGDD_May1 = gddCalc(tmean, base = 0, asOfJD = 121),
            daymetGDD_Jun1 = gddCalc(tmean, base = 0, asOfJD = 152),
            date2000GDD = dateOfGDDaccumulation(tmean, base = 0, accumulateTo = 2000),
            precipThruJun1 = sum(prcp..mm.day.[yday <= 152], na.rm = T))


################################################################
# GDD and NPN spring index
#
siteyears = fullDataset %>% 
  filter(!Name %in% c('Example Site', Name[grepl('BBS', Name)])) %>%
  distinct(Name, Year, Longitude, Latitude, medianGreenup, ebirdCounty)

leafyears = 2002:2019

leafout = vector("list", length(leafyears))
for (i in 1:length(leafyears)) {
  
  npnleaf = raster(paste0('data/maps/npn_springindex_firstleaf_', leafyears[i], '.tif'))
  leafout[[i]] = filter(siteyears, Year == leafyears[i]) %>%
    mutate(npn_firstLeaf = extract(npnleaf, cbind.data.frame(Longitude, Latitude)))
  
}
leaf = bind_rows(leafout)

gddyears = 2016:2019
gddout = vector("list", length(gddyears))
for (i in 1:length(gddyears)) {
  
  npngdd = raster(paste0('data/maps/npn_gdd_base32_to_may1_', gddyears[i], '.tif'))
  gddout[[i]] = filter(siteyears, Year == gddyears[i]) %>%
    mutate(npnGDD_May1 = extract(npngdd, cbind.data.frame(Longitude, Latitude)))
  
}
gdd = bind_rows(gddout)

envData = left_join(leaf, gdd[, c('Name', 'Year', 'npnGDD_May1')], by = c('Name', 'Year')) %>% 
  left_join(daymetGDD, by = c('Name', 'Year' = 'year'))

write.csv(envData, 'data/env/npn_gdd_firstleaf.csv', row.names = F)
