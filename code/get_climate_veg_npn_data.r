# This file calculates climate and vegetation related summaries for Caterpillars Count! sites

library(dplyr)
library(lubridate)
library(raster)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')


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

envData = left_join(leaf, gdd[, c('Name', 'Year', 'npnGDD_May1')], by = c('Name', 'Year'))

write.csv(envData, 'data/env/npn_gdd_firstleaf.csv', row.names = F)
