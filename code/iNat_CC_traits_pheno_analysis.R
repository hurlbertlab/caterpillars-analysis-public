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
inat_traits$defended <- ifelse(inat_traits$hairy == "Y", "Y",
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
    group_by(lat_bin, defended,jd_wk) %>%
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
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "defended"), .funs = change1) %>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "defended"), .funs = as.numeric)

inat_traits_ones_zeros <- inat_traits%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "hs","defended"), .funs = change0)

inat_species_numeric <- inat_species%>%
  mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent","aggregate", "social"), .funs = change0)
  
inat_species_numeric$hs <- ifelse(inat_species_numeric$hairy == 1, 1,
                                  ifelse(inat_species_numeric$spiny == 1, 1, 0))


#intermediate bins to get proportions for each latitudinal band
two_deg <- recsByBinTrait(inat_traits, 2)%>%
  filter(lat_bin >= 29, lat_bin <=49)
  
twentynine_y <- two_deg%>%
  filter(lat_bin == 29, defended == "Y")
twentynine_y$total = sum(twentynine_y$n)
twentynine_y$proportion = (twentynine_y$n/twentynine_y$total)*100

twentynine_n <- two_deg%>%
  filter(lat_bin == 29, defended == "N")
twentynine_n$total = sum(twentynine_n$n)
twentynine_n$proportion = (twentynine_n$n/twentynine_n$total)*100

thirtyone_y <- two_deg%>%
  filter(lat_bin == 31, defended == "Y")
thirtyone_y$total = sum(thirtyone_y$n)
thirtyone_y$proportion = (thirtyone_y$n/thirtyone_y$total)*100

thirtyone_n <- two_deg%>%
  filter(lat_bin == 31, defended == "N")
thirtyone_n$total = sum(thirtyone_n$n)
thirtyone_n$proportion = (thirtyone_n$n/thirtyone_n$total)*100

thirtythree_y <- two_deg%>%
  filter(lat_bin == 33, defended == "Y")
thirtythree_y$total = sum(thirtythree_y$n)
thirtythree_y$proportion = (thirtythree_y$n/thirtythree_y$total)*100

thirtythree_n <- two_deg%>%
  filter(lat_bin == 33, defended == "N")
thirtythree_n$total = sum(thirtythree_n$n)
thirtythree_n$proportion = (thirtythree_n$n/thirtythree_n$total)*100

thirtyfive_y <- two_deg%>%
  filter(lat_bin == 35, defended == "Y")
thirtyfive_y$total = sum(thirtyfive_y$n)
thirtyfive_y$proportion = (thirtyfive_y$n/thirtyfive_y$total)*100

thirtyfive_n <- two_deg%>%
  filter(lat_bin == 35, defended == "N")
thirtyfive_n$total = sum(thirtyfive_n$n)
thirtyfive_n$proportion = (thirtyfive_n$n/thirtyfive_n$total)*100

thirtyseven_y <- two_deg%>%
  filter(lat_bin == 37, defended == "Y")
thirtyseven_y$total = sum(thirtyseven_y$n)
thirtyseven_y$proportion = (thirtyseven_y$n/thirtyseven_y$total)*100

thirtyseven_n <- two_deg%>%
  filter(lat_bin == 37, defended == "N")
thirtyseven_n$total = sum(thirtyseven_n$n)
thirtyseven_n$proportion = (thirtyseven_n$n/thirtyseven_n$total)*100

thirtynine_y <- two_deg%>%
  filter(lat_bin == 39, defended == "Y")
thirtynine_y$total = sum(thirtynine_y$n)
thirtynine_y$proportion = (thirtynine_y$n/thirtynine_y$total)*100

thirtynine_n <- two_deg%>%
  filter(lat_bin == 39, defended == "N")
thirtynine_n$total = sum(thirtynine_n$n)
thirtynine_n$proportion = (thirtynine_n$n/thirtynine_n$total)*100

fortyone_y <- two_deg%>%
  filter(lat_bin == 41, defended == "Y")
fortyone_y$total = sum(fortyone_y$n)
fortyone_y$proportion = (fortyone_y$n/fortyone_y$total)*100

fortyone_n <- two_deg%>%
  filter(lat_bin == 41, defended == "N")
fortyone_n$total = sum(fortyone_n$n)
fortyone_n$proportion = (fortyone_n$n/fortyone_n$total)*100

fortythree_y <- two_deg%>%
  filter(lat_bin == 43, defended == "Y")
fortythree_y$total = sum(fortythree_y$n)
fortythree_y$proportion = (fortythree_y$n/fortythree_y$total)*100

fortythree_n <- two_deg%>%
  filter(lat_bin == 43, defended == "N")
fortythree_n$total = sum(fortythree_n$n)
fortythree_n$proportion = (fortythree_n$n/fortythree_n$total)*100

fortyfive_y <- two_deg%>%
  filter(lat_bin == 45, defended == "Y")
fortyfive_y$total = sum(fortyfive_y$n)
fortyfive_y$proportion = (fortyfive_y$n/fortyfive_y$total)*100

fortyfive_n <- two_deg%>%
  filter(lat_bin == 45, defended == "N")
fortyfive_n$total = sum(fortyfive_n$n)
fortyfive_n$proportion = (fortyfive_n$n/fortyfive_n$total)*100

fortyseven_y <- two_deg%>%
  filter(lat_bin == 47, defended == "Y")
fortyseven_y$total = sum(fortyseven_y$n)
fortyseven_y$proportion = (fortyseven_y$n/fortyseven_y$total)*100

fortyseven_n <- two_deg%>%
  filter(lat_bin == 47, defended == "N")
fortyseven_n$total = sum(fortyseven_n$n)
fortyseven_n$proportion = (fortyseven_n$n/fortyseven_n$total)*100

fortynine_y <- two_deg%>%
  filter(lat_bin == 49, defended == "Y")
fortynine_y$total = sum(fortynine_y$n)
fortynine_y$proportion = (fortynine_y$n/fortynine_y$total)*100

fortynine_n <- two_deg%>%
  filter(lat_bin == 49, defended == "N")
fortynine_n$total = sum(fortynine_n$n)
fortynine_n$proportion = (fortynine_n$n/fortynine_n$total)*100

binned_latitude <- rbind(twentynine_n, twentynine_y, thirtyone_n, thirtyone_y, 
                         thirtythree_n, thirtythree_y, thirtyfive_n, thirtyfive_y, 
                         thirtyseven_n, thirtyseven_y, thirtynine_n, thirtynine_y, 
                         fortyone_n, fortyone_y, fortythree_n, fortythree_y, fortyfive_n,
                         fortyfive_y, fortyseven_n, fortyseven_y, fortynine_n, fortynine_y)
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

  
  