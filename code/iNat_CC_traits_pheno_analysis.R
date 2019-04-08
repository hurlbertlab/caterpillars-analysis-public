# Examining caterpillar traits and phenology with iNaturalist/Caterpillars Count! data

#### Libraries ####
library(tidyverse)
library(lubridate)
library(ggplot2)

#### Read in Data ####
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

inat_species = read.table("data/inat_species.txt", header = T, sep = "\t",quote = "", fill = FALSE)

source('code/analysis_functions.r')
source('code/CCrawdata2masterdataframe.r')

# Aggregate iNat data by day, week, year
inat$observed_on = as.Date(inat$observed_on, format = "%Y-%m-%d")
inat$year = format(inat$observed_on, format = "%Y")
inat$jday = yday(inat$observed_on)
inat$jd_wk = 7*floor(inat$jday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

inat$lat_round = round(inat$latitude, 1)
inat$lon_round = round(inat$longitude, 1)

# Join iNat Records with Traits on Scientific Name 
# Filter observations not identified to species
# Filter observations outside of breeding season
jdBeg = 1
jdEnd = 182 

# Main Dataframes
inat_traits <- inat %>%
  left_join(inat_species, by = 'scientific_name') %>%
  filter(genus != "NA",jday >= jdBeg, jday <= jdEnd)


CC_traits<- fullDataset%>%
  filter(Group == "caterpillar")

CC_traits$Defended <- ifelse(CC_traits$Hairy == 1, 1, 
                             ifelse(CC_traits$Rolled == 1, 1, 
                                    ifelse(CC_traits$Tented == 1, 1, 0)))

# add column denoting if caterpillar is defended 
inat_traits$Trait <- ifelse(inat_traits$hairy == "Y", "Y",
                               ifelse(inat_traits$spiny == "Y", "Y",
                                      ifelse(inat_traits$leafroll == "Y", "Y",
                                             #ifelse(inat_traits$aposematic == "Y", "Y",
                                                    ifelse(inat_traits$silktent == "Y", "Y", "N"))))

# changes in traits by year caterpillars 
defended_by_year <- inat_traits %>%
  group_by(year, jd_wk, defended) %>%
  count() %>%
  filter(year>=2015)

#combine hairy and spiny column
inat_traits_ones$hs <- ifelse(inat_traits_ones$hairy == 1, 1, 
                              ifelse(inat_traits_ones$spiny == 1, 1, .))


inat_traits$hs <- ifelse(inat_traits$hairy == "Y", "Y", 
                         ifelse(inat_traits$spiny == "Y", "Y", "N" ))
#### Functions ####

# Count number of records per Defended/Undefended and lat-long bin as a function of bin size (in degrees)
recsByBinTrait = function(df, binsize) {
  df$lat_bin = binsize*floor(df$latitude/binsize) + binsize/2
  df$lon_bin = binsize*floor(df$longitude/binsize) + binsize/2
  
  inat_by_latlon_Trait = df %>%
    group_by(lat_bin, Trait,jd_wk) %>%
    count()
  
  return(inat_by_latlon_Trait)
}

# change Y to 1's
change1 <- function(x) {
  
  gsub("Y", 1, x)
}

# change N to 0's
change0 <- function(x) {
  ifelse(x == "Y", 1, 0)
}

inat_traits_ones <- inat_traits%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "Trait"), .funs = change1) %>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "Trait"), .funs = as.numeric)

inat_traits_ones_zeros <- inat_traits%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "hs","Trait"), .funs = change0)

inat_species_numeric <- inat_species%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent","aggregate", "social"), .funs = change0)
  
inat_species_numeric$hs <- ifelse(inat_species_numeric$hairy == 1, 1,
                                  ifelse(inat_species_numeric$spiny == 1, 1, 0))



two_deg <- recsByBinTrait(inat_traits, 2)
  

#counts of traits
Counts_traits <- inat_species_numeric%>%
  group_by(hs, leafroll, aposematic,silktent)%>%
  count()
  

  
#### Plots ####

ggplot(hs_wk_year, aes(x=jday,y=sum)) +
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
ggsave("figs/iNat_Defended_Caterpillars_2015-2018.pdf", width = 12, height = 8, units = "in")
  
ggplot(Counts_traits, aes(x=))

  
  