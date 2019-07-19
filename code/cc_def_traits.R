library(dplyr)
library(lubridate)
library(tidyverse)
library(maps)
library(usmap)
library(ggplot2)


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
  
  sites = read.csv(paste('C:/git/caterpillars-analysis-public/data/2019-06-12_Site.csv', list.files('data')[grep('2019-06-12_Site.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
  
  surveys = read.csv(paste('C:/git/caterpillars-analysis-public/data/2019-06-12_Survey.csv', list.files('data')[grep('2019-06-12_Survey.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
  
  arths = read.csv(paste('C:/git/caterpillars-analysis-public/data/2019-06-12_ArthropodSighting.csv', list.files('data')[grep('2019-06-12_ArthropodSighting.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
  
  plants = read.csv(paste('C:/git/caterpillars-analysis-public/data/2019-06-12_Plant.csv', list.files('data')[grep('2019-06-12_Plant.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
  
  surveys$LocalDate = as.Date(surveys$LocalDate, format = "%Y-%m-%d")
  surveys$Year = format(surveys$LocalDate, "%Y")
  surveys$julianday = yday(surveys$LocalDate)
  
  
  fullDataset = surveys %>%
    dplyr::select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, Year, ObservationMethod, Notes, WetLeaves, PlantSpecies, NumberOfLeaves,
                  AverageLeafLength, HerbivoryScore) %>%
    left_join(arths[, !names(arths) %in% "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
    left_join(plants, by = c('PlantFK' = 'ID')) %>%
    left_join(sites[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region')], by = c('SiteFK' = 'ID')) %>% 
    mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
    rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y)
  
  
  jdBeg = 50
  jdEnd = 211 

  
sep_cat<-fullDataset%>%
  filter(arthID != "NA",Group=='caterpillar',julianday >= jdBeg, julianday <= jdEnd)

sep_cat$defended<-ifelse(sep_cat$Hairy=="1","Y",
                         ifelse(sep_cat$Rolled=="1","Y",
                                ifelse(sep_cat$Tented=="1","Y","N"))
                         )

hairy_cat<-sep_cat%>%
            filter(Hairy=="1")
rolled_cat<-sep_cat%>%
            filter(Rolled=="1")
tent_cat<-sep_cat%>%
            filter(Tented=="1")
undefend_cat<-sep_cat%>%
  filter(Undefended=="1")



plot_usmap(include = c("NC", "MD", "VA", "NY", "FL")) +
  labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.")
usa<-map_data("usa")
states<-map_data("state")
ggplot(data=states)+geom_polygon(aes(x=long,y=lat,fill=region,group=group),color="white")+coord_fixed(1.3)+guides(fill=FALSE)


sep_cat$Undefended<-as.integer(ifelse(sep_cat$defended=="Y","0","1"))

defense_by_site <- sep_cat %>%
  group_by(Latitude, Longitude, Name) %>%
  summarize(n = n(),
            pct_hairy = sum(Hairy)/n,
            pct_roll = sum(Rolled)/n,
            pct_tent = sum(Tented)/n,
            pct_undefend=sum(Undefended)/n)%>%
            filter(Latitude>1, n > 20)

defense_by_site_only<-sep_cat %>%
            group_by(Name) %>%
            summarize(n = n(),
            pct_hairy = sum(Hairy)/n,
            pct_roll = sum(Rolled)/n,
            pct_tent = sum(Tented)/n,
            pct_undefend=sum(Undefended)/n)%>%
             filter( n > 20)
defense_by_site_wide<-spread(defense_by_site_only,Name,pct_undefend)

as.numeric(as.character(defense_by_site$pct_undefend))
as.numeric(as.character(defense_by_site$pct_hairy))
as.numeric(as.character(defense_by_site$pct_tent))
as.numeric(as.character(defense_by_site$pct_roll))



Hairy_agg=aggregate(defense_by_site$pct_hairy,
              by=list(defense_by_site$Name),
              FUN=sum)


Roll_agg=aggregate(defense_by_site$pct_roll,
                    by=list(defense_by_site$Name),
                    FUN=sum)

Tent_agg=aggregate(defense_by_site$pct_tent,
                    by=list(defense_by_site$Name),
                    FUN=sum)

Undefend_agg=aggregate(defense_by_site$pct_undefend,
                    by=list(defense_by_site$Name),
                    FUN=sum)

Hairy_wide<-spread(Hairy_agg,Group.1,x)
Roll_wide<-spread(Roll_agg,Group.1,x)
Tent_wide<-spread(Tent_agg,Group.1,x)
Undefend_wide<-spread(Undefend_agg,Group.1,x)

Combine<-rbind(Hairy_wide,Roll_wide,Tent_wide,Undefend_wide)
Combined_barplot<-barplot(as.matrix(Combine))



summarize(Combine)
  defense_by_site%>%
  group_by(Name)%>%
  filter(n>20)%>%
  summarize(n())

