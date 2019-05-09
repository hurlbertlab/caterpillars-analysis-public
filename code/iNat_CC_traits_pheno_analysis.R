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
jdBeg = 50
jdEnd = 211 

# Main Dataframes
inat_traits <- inat %>%
  left_join(inat_species, by = 'scientific_name') %>%
  filter(genus != "NA",jday >= jdBeg, jday <= jdEnd)

inat_traits$year <- as.numeric(as.character(inat_traits$year))


# add column denoting if caterpillar is defended 
inat_traits$defended <- ifelse(inat_traits$hairy == "Y", "Y",
                               ifelse(inat_traits$spiny == "Y", "Y", 
                                      ifelse(inat_traits$leafroll == "Y", "Y",
                                             ifelse(inat_traits$aposematic == "Y", "Y", 
                                                    ifelse(inat_traits$silktent == "Y", "Y", "N")))))

# changes in traits by year caterpillars 
#defended_by_year <- inat_traits %>%
  #group_by(year, jd_wk, defended) %>%
  #count() %>%
  #filter(year>=2015)

#combine hairy and spiny column
#inat_traits_ones$hs <- ifelse(inat_traits_ones$hairy == 1, 1, 
                              #ifelse(inat_traits_ones$spiny == 1, 1, .))


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
# Same as previous function, groups by year as well
recsByBinTraitYear = function(df, binsize) {
  df$lat_bin = binsize*floor(df$latitude/binsize) + binsize/2
  df$lon_bin = binsize*floor(df$longitude/binsize) + binsize/2
  
  inat_by_latlon_Trait = df %>%
    group_by(year, lat_bin, defended, jd_wk) %>%
    count()
  
  return(inat_by_latlon_Trait)
}

# same as previous function seeking different output
recsByBin = function(df, binsize) {
  df$lat_bin = binsize*floor(df$latitude/binsize) + binsize/2
  df$lon_bin = binsize*floor(df$longitude/binsize) + binsize/2
  
  inat_by_latlon_Trait = df
  
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

# Change that fam #
changeFAM <- function(x){
  ifelse(x == 'Erebidae', 'Erebidae',
         ifelse(x== 'Nymphalidae', 'Nymphalidae',
                ifelse(x == 'Papilionidae','Papilionidae',
                       ifelse(x == 'Noctuidae', 'Noctuidae', 
                              ifelse(x == 'Lasiocampidae', 'Lasiocampidae', 
                                     ifelse(x== 'Saturniidae', 'Saturniidae',
                                            ifelse(x == 'Sphingidae', 'Sphingidae',
                                                   ifelse(x == 'Geometridae', 'Geometridae',
                                                          ifelse(x == 'Notodontidae', 'Notodontidae', 'Other')))))))))
}
#change Na's#

# change
#inat_traits_ones <- inat_traits%>%
  #mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "defended"), .funs = change1) %>%
  #mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "defended"), .funs = as.numeric)

#inat_traits_ones_zeros <- inat_traits%>%
  #mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent", "hs","defended"), .funs = change0)

#inat_species_numeric <- inat_species%>%
  #mutate_at(.vars = c("hairy", "spiny","aposematic", "leafroll","silktent","aggregate", "social"), .funs = change0)
  
#inat_species_numeric$hs <- ifelse(inat_species_numeric$hairy == 1, 1,
                                  #ifelse(inat_species_numeric$spiny == 1, 1, 0))

#### Forming latitudinal bins and gleaning proportions ####
two_deg <- recsByBinTrait(inat_traits, 2)%>%
  filter(lat_bin >= 29, lat_bin <=49)


binned_latitude <- two_deg%>%
  group_by(lat_bin, defended)%>%
  mutate(total = sum(n))%>%
  mutate(proportion = (n/total)*100)
  
# Create a text string showing the number of records of undefended, defended caterpillars per grid cell
nRecs_by_latitude1 = binned_latitude %>%
  distinct(lat_bin, total, defended) %>%
  mutate(x = ifelse(defended == "N", 75, 200),
         y = 32)

nRecs_by_latitude = binned_latitude %>%
  distinct(lat_bin, total, defended) %>%
  group_by(lat_bin) %>%
  mutate(text = paste(total[defended == "N"], ", ", total[defended == "Y"], sep = "")) %>%
  distinct(lat_bin, text)


#### df for linear model ####
jd_wk_model <- recsByBin(inat_traits, 2)%>%
  filter(lat_bin >= 29, lat_bin <=49)%>%
  filter(year >= 2015)

peak_wk_model <- recsByBin(inat_traits, 2)%>%
  filter(lat_bin >= 29, lat_bin <= 49)%>%
  filter(year >= 2015)%>%
  group_by(year, lat_bin, defended, jd_wk)%>%
  count()%>%
  ungroup()%>%
  group_by(year, lat_bin, defended)%>%
  filter(n == max(n))%>%
  group_by(year, lat_bin, defended)%>%
  filter(jd_wk == min(jd_wk))
#### df for graphs of defended and undefended over time per year ####
two_deg_year <- recsByBinTraitYear(inat_traits,2)%>%
  filter(lat_bin >= 29, lat_bin <=49)%>%
  filter(year >= 2015)

binned_lat_year <- two_deg_year%>%
  group_by(year, lat_bin, defended)%>%
  mutate(total = sum(n))%>%
  mutate(proportion = n/total)
  

LessBins_lat_year <- two_deg_year%>%
  group_by(year, lat_bin, defended)%>%
  mutate(total = sum(n))%>%
  mutate(proportion = n/total)%>%
  filter(year >= 2015)%>%
  filter(lat_bin != 29, lat_bin != 33, lat_bin != 37, lat_bin != 41, lat_bin != 45, lat_bin != 49)
 
  
nRecs_by_lat_year = LessBins_lat_year %>%
  distinct(lat_bin, year, total, defended) %>%
  mutate(x = ifelse(defended == "N", 75, 200),
         y = .45)


  
#### df for bar charts ####
families <- inat_traits%>%
  mutate_at(.vars = c("family"), .funs = changeFAM)%>%
  mutate(family = coalesce(family, 'Other'))%>%
  group_by(family, defended)%>%
  count()%>%
  ungroup()%>%
  group_by(family)%>%
  mutate(total = sum(n))%>%
  mutate(proportion = n/total)



# review of number of obs. per family #
count_of_families <- inat_traits%>%
  group_by(family)%>%
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
  
ggplot(binned_latitude, aes(x= jd_wk, y = proportion, color = defended))+
  geom_line()+
  ggtitle("Percent of Defended and Undefended Caterpillars Over Time")+
  ylab("Percent of Caterpillars")+
  xlab("Julian Week")+
  facet_wrap(~lat_bin)+
  geom_text(data = nRecs_by_latitude1, 
            mapping = aes(x = x, y = y, label = total))


ggplot(LessBins_lat_year, aes(x = jd_wk, y= proportion, color = defended))+
  geom_line()+
  xlab("Julian Day")+
  ylab("Proportion of Caterpillars")+
  ggtitle("Seasonal Changes in Proportion of Defended and Undefended Caterpillars \n Over Latitude and Between Years \n 2015-2018")+
  facet_grid(lat_bin~year)+
  geom_text(data = nRecs_by_lat_year, 
            mapping = aes(x = x, y = y, label = total))

ggplot(families, aes(x = family, y = n, color = defended, fill = defended))+
  geom_bar(stat = "identity")+
  xlab("Family")+
  ylab("Number of Observations")+
  ggtitle("Number of Observations for Nine Most Common 
          Caterpillar Families (2015-2018) ")+
  coord_flip()

ggplot(families, aes(x= family, y=proportion, color = defended, fill = defended))+
  geom_bar(stat = "identity")+
  xlab("Family")+
  ylab("Proportion")+
  ggtitle("Proportions of Defended and Undefended Caterpillars 
            for Nine Common Families")+
  coord_flip()

#### modeling ####

# predicting jd_wk #
mod1 <- lm(jd_wk~lat_bin + defended + defended * lat_bin, jd_wk_model)
summary(mod1)

mod2 <- lm(jd_wk~lat_bin + defended + defended *lat_bin, peak_wk_model)
summary(mod2)  

# df for predicted  date model values #
mod1_df_defended <- data.frame("lat_bin" = c(29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 ))%>%
  mutate(defended = 1)%>%
  mutate(y_n = "Y")%>%
  mutate(predicted_week = 58.9154 + 2.8111*lat_bin-20.0613+0.3379*lat_bin)

mod1_df_undefended <- data.frame("lat_bin" = c(29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 ))%>%
  mutate(defended = 0)%>%
  mutate(y_n = "N")%>%
  mutate(predicted_week = 58.9154 + 2.8111*lat_bin)

final_mod1_df <- rbind(mod1_df_defended, mod1_df_undefended)
# graph of predicted values from model #
ggplot(final_mod1_df, aes(x = lat_bin, y = predicted_week, color = y_n))+
  geom_line()

# df for peak date prediction #
mod2_df_def<- data.frame("lat_bin" = c(29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 ))%>%
  mutate(defended = 1)%>%
  mutate(y_n = "Y")%>%
  mutate(predicted_peak = 58.3019 + 2.7533*lat_bin-39.3156+0.7785*lat_bin)

mod2_df_undef<- data.frame("lat_bin" = c(29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 ))%>%
  mutate(defended = 0)%>%
  mutate(y_n = "N")%>%
  mutate(predicted_peak = 58.3019 + 2.7533*lat_bin)

final_mod2_df <- rbind(mod2_df_def, mod2_df_undef)

ggplot(final_mod2_df, aes(x = lat_bin, y = predicted_peak, color = y_n))+
  geom_line()
#### replication with Caterpillars Count ####
CC_traits<- fullDataset%>%
  filter(Group == "caterpillar")

CC_traits$Defended <- ifelse(CC_traits$Hairy == 1, 1, 
                             ifelse(CC_traits$Rolled == 1, 1, 
                                    ifelse(CC_traits$Tented == 1, 1, 0)))


