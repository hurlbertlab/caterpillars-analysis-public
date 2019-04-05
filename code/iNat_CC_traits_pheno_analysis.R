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

# add column denoting if caterpillar is defended 
inat_traits$defended <- ifelse(inat_traits$hairy == "Y", "Y",
                               ifelse(inat_traits$spiny == "Y", "Y",
                                      ifelse(inat_traits$leafroll == "Y", "Y",
                                             ifelse(inat_traits$aposematic == "Y", "Y",
                                                    ifelse(inat_traits$silktent == "Y", "Y", "N")))))

# changes in traits by year caterpillars 

defended_by_year <- inat_traits %>%
  group_by(year, jd_wk, defended) %>%
  count() %>%
  #count(spiny)%>%
  filter(year>=2015)

change1 <- function(x) {
  gsub("Y", 1, x)
}

traits_by_year <- inat_traits%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent"), .funs = change1) %>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent"), .funs = as.numeric)

#### Plots ####
ggplot(data=traits_by_year, aes(x=jd_wk,y=n, color = hairy)) +
 geom_line() +
 # geom_point() +
  #geom_col() +
  #geom_area() +
  facet_wrap(~year)

ggplot(defended_by_year, aes(x=jd_wk, y=n, color = defended, fill = defended)) + 
  geom_area() + 
  ggtitle("Plot of Proportion of Defended and Undefended Caterpillars by Year")+
  xlab("Julian Week")+
  ylab("Number of Caterpillars")+
  facet_wrap(~year)
  
