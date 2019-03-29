# Examining caterpillar traits and phenology with iNaturalist/Caterpillars Count! data

#### Libraries ####
library(tidyverse)
library(taxize)
library(lubridate)
library(ggplot2)

#### Read in Data ####
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

inat_species = read.table("data/inat_species.txt", header = T, sep = "\t",quote = "", fill = FALSE)

source('code/analysis_functions.r')
source('code/CCrawdata2masterdataframe.r')

# Aggregate iNat data by day, year, and lat-lon bin
inat$observed_on = as.Date(inat$observed_on, format = "%Y-%m-%d")
inat$year = format(inat$observed_on, format = "%Y")
inat$jday = yday(inat$observed_on)
inat$jd_wk = 7*floor(inat$jday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

# Join iNat Records with Traits on Scientific Name 
# Filter observations not identified to species
# Filter observations outside of breeding season
jdBeg = 91
jdEnd = 240 

inat_traits <- inat %>%
  left_join(inat_species, by = 'scientific_name') %>%
  filter(genus != "NA",jday >= jdBeg, jday <= jdEnd)

# changes in traits by year caterpillars 

traits_by_year <- inat_traits %>%
  group_by(year, jd_wk, hairy, spiny) %>%
  count() %>%
  filter(year>=2015)

ggplot(data=traits_by_year, aes(x=jd_wk,y=n,color=hairy,fill=hairy)) +
#  geom_line() +
 # geom_point() +
  #geom_col() +
  geom_area() +
  facet_wrap(~year)
  
