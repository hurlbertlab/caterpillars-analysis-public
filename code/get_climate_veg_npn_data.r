# This file calculates climate and vegetation related summaries for Caterpillars Count! sites

library(dplyr)
library(lubridate)
library(raster)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')


################################################################
# GDD and NPN spring index
#

npnGDD16 = raster('data/maps/npn_gdd_base32_to_may1_2016.tif')
npnGDD17 = raster('data/maps/npn_gdd_base32_to_may1_2017.tif')
npnGDD18 = raster('data/maps/npn_gdd_base32_to_may1_2018.tif')
npnGDD19 = raster('data/maps/npn_gdd_base32_to_may1_2019.tif')

npnleaf15 = raster('data/maps/npn_springindex_firstleaf_2015.tif')
npnleaf16 = raster('data/maps/npn_springindex_firstleaf_2016.tif')
npnleaf17 = raster('data/maps/npn_springindex_firstleaf_2017.tif')
npnleaf18 = raster('data/maps/npn_springindex_firstleaf_2018.tif')
npnleaf19 = raster('data/maps/npn_springindex_firstleaf_2019.tif')

siteyears = fullDataset %>% 
  filter(!Name %in% c('Example Site', Name[grepl('BBS', Name)])) %>%
  distinct(Name, Year, Longitude, Latitude, medianGreenup, ebirdCounty)

years = 2016:2019

out = vector("list", length(years))
for (i in 1:length(years)) {
  
  out[[i]] = filter(siteyears, Year == years[i]) %>%
    mutate(npnGDD_May1 = extract(get(paste0('npnGDD', substr(years[i], 3, 4))), cbind.data.frame(Longitude, Latitude)),
           npn_firstLeaf = extract(get(paste0('npnleaf', substr(years[i], 3, 4))), cbind.data.frame(Longitude, Latitude)))
  
}

envData = bind_rows(out)
# For 2015, only spring index, but not GDD data are available, 
# so calc separately and then rbind

leaf2015 = filter(siteyears, Year == 2015) %>%
  mutate(npnGDD_May1 = NA,
         npn_firstLeaf = extract(npnleaf15, cbind.data.frame(Longitude, Latitude)))

envData = rbind(leaf2015, envData)         

write.csv(envData, 'data/env/npn_gdd_firstleaf.csv', row.names = F)
