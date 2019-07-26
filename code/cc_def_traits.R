library(dplyr)
library(lubridate)
library(tidyverse)
library(maps)
library(usmap)
extrafont::loadfonts(device="pdf")
extrafont::loadfonts(device="postscript")
library(ggplot2)
library(stringr)
library(gridExtra)
library(plyr)



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
    mutate_cond(is.na(Quantity), Quantity = 0, Group)
   # rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y)
  
  jdBeg = 50
  jdEnd = 211 

  
sep_cat<-fullDataset%>%
  filter(ID.y != "NA",Group=='caterpillar')

sep_cat$defended<-ifelse(sep_cat$Hairy=="1","Y",
                         ifelse(sep_cat$Rolled=="1","Y",
                                ifelse(sep_cat$Tented=="1","Y","N"))
                         )
sep_cat$Undefended<-as.integer(ifelse(sep_cat$defended=="Y","0","1"))
hairy_cat<-sep_cat%>%
            filter(Hairy=="1")
rolled_cat<-sep_cat%>%
            filter(Rolled=="1")
tent_cat<-sep_cat%>%
            filter(Tented=="1")
undefend_cat<-sep_cat%>%
  filter(Undefended=="1")


PRpheno<-sep_cat%>%
  group_by(Name,julianday,Year) %>%
  dplyr::summarize(n = n(),
            pct_defend = sum(Hairy,Rolled,Tented)/n,
            pct_undefend=sum(Undefended)/n)%>%
  filter(n>10,Year>2015,Name=="Prairie Ridge Ecostation")

NCBGpheno<-sep_cat%>%
  group_by(Name,julianday,Year) %>%
  dplyr::summarize(n = n(),
                   pct_defend = sum(Hairy,Rolled,Tented)/n,
                   pct_undefend=sum(Undefended)/n)%>%
  filter(n>5,Year>2015,Name=="NC Botanical Garden")

defense_by_site <- sep_cat %>%
  group_by(Latitude, Longitude, Name) %>%
  dplyr::summarize(n = n(),
            pct_hairy = sum(Hairy)/n,
            pct_roll = sum(Rolled)/n,
            pct_tent = sum(Tented)/n,
            pct_undefend=sum(Undefended)/n)%>%
            filter( n > 20)


defense_by_site_only<-sep_cat %>%
            group_by(Name) %>%
           dplyr:: summarize(n = n(),
            pct_hairy = sum(Hairy)/n,
            pct_roll = sum(Rolled)/n,
            pct_tent = sum(Tented)/n,
            pct_undefend=sum(Undefended)/n)%>%
             filter( n > 20)
  

Defense_by_site_order_Spiky<-defense_by_site_only[order(defense_by_site_only$pct_hairy),]


defense_by_site_wide<-spread(defense_by_site_only,Name,pct_undefend)

as.numeric(as.character(defense_by_site$pct_undefend))
as.numeric(as.character(defense_by_site$pct_hairy))
as.numeric(as.character(defense_by_site$pct_tent))
as.numeric(as.character(defense_by_site$pct_roll))


#Aggregate barplot ordered by % Hairy
# Hairy_agg<-aggregate(Defense_by_site_order_Spiky$pct_hairy,
#               by=list(Defense_by_site_order_Spiky$Name),
#               FUN=sum)
# 
# 
# Roll_agg<-aggregate(Defense_by_site_order_Spiky$pct_roll,
#                     by=list(Defense_by_site_order_Spiky$Name),
#                     FUN=sum)
# 
# Tent_agg<-aggregate(Defense_by_site_order_Spiky$pct_tent,
#                     by=list(Defense_by_site_order_Spiky$Name),
#                     FUN=sum)
# 
# Undefend_agg<-aggregate(Defense_by_site_order_Spiky$pct_undefend,
#                     by=list(Defense_by_site_order_Spiky$Name),
#                     FUN=sum)
#Aggregate barplot ordered by Latitude

# Hairy_Lat=aggregate(defense_by_site$pct_hairy,
#                     by=list(defense_by_site$Latitude),
#                     FUN=sum)
# 
# 
# Roll_Lat=aggregate(defense_by_site$pct_roll,
#                    by=list(defense_by_site$Latitude),
#                    FUN=sum)
# 
# Tent_Lat=aggregate(defense_by_site$pct_tent,
#                    by=list(defense_by_site$Latitude),
#                    FUN=sum)
# 
# Undefend_Lat=aggregate(defense_by_site$pct_undefend,
#                        by=list(defense_by_site$Latitude),
#                        FUN=sum)
# 
# Hairy_agg_order<-Hairy_agg[order(Hairy_agg$x),]
# 
# 
# Hairy_wide<-spread(Hairy_agg,Group.1,x)
# Roll_wide<-spread(Roll_agg,Group.1,x)
# Tent_wide<-spread(Tent_agg,Group.1,x)
# Undefend_wide<-spread(Undefend_agg,Group.1,x)
# 
# Hairy_Lat_wide<-spread(Hairy_Lat,Group.1,x)
# Roll_Lat_wide<-spread(Roll_Lat,Group.1,x)
# Tent_Lat_wide<-spread(Tent_Lat,Group.1,x)
# Undefend_Lat_wide<-spread(Undefend_Lat,Group.1,x)
# 
# 
# Combine<-rbind(Hairy_wide,Roll_wide,Tent_wide,Undefend_wide)
# arrange(Combine,Combine$`Environmental Sciences Magnet School`,desc)
# Combine_Lat<-rbind(Hairy_Lat_wide,Roll_Lat_wide,Tent_Lat_wide,Undefend_Lat_wide)
# 
# 


par(mar = c(5,3,1,1))
par(mfrow=c(1,2))

# Combined_barplot<-barplot(as.matrix(Combine),
#                           las = 1, 
#                           col = c('steelblue2', 'snow4', 'tomato3', 'mintcream'), 
#                           xaxt = "n")
# text(Combined_barplot, rep(-.15, ncol(Combine)), strtrim(names(Combine),5),srt = 45, xpd = TRUE, adj = 1)
# 
# Barplot_lat<-barplot(as.matrix(Combine_Lat),
#                           las = 1, 
#                           col = c('dodgerblue', 'seagreen', 'orangered', 'yellow'), 
#                           xaxt = "n")
# text(Barplot_lat,rep(-.15,ncol(Combine_Lat)),strtrim(names(Combine_Lat),4), srt=45,xpd=TRUE,adj=1)
# 
# 
# plot<-ggplot()+geom_bar(aes(x=Name, y=pct_hairy,fill="hairy"),data=Defense_by_site_order_Spiky,stat="identity",position="stack")+
#               geom_bar(aes(x=Name,y=pct_roll,fill="roll"),data=Defense_by_site_order_Spiky,stat="identity",position="stack")+
#                 geom_bar(aes(x=Name,y=pct_tent,fill="tent"),data=Defense_by_site_order_Spiky,stat="identity",position="stack")+
#                 geom_bar(aes(x=Name,y=pct_undefend,fill="undefend"),data=Defense_by_site_order_Spiky,stat="identity",position="stack")+
#                                       theme(axis.text.x = element_text( color="black", 
#                                       size=9, angle=45,vjust=1,hjust=1),
#                                        axis.text.y = element_text( color="black", 
#                                         size=7, angle=45))+theme(plot.margin=margin(1,40,20,1))
# plot<-ggplot(data=Defense_by_site_order_Spiky,aes(x=Name,y=pct_hairy,fill="hairy"))+geom_bar(stat="identity")
# plot

test <- Defense_by_site_order_Spiky %>%
  gather(trait, pct, 3:6) %>%
  group_by(Name) %>%
  mutate(order = pct[trait == "pct_undefend"])

site_code<-ifelse(test$Name=="BBS 27-041-32","BBS 27",
                    ifelse(test$Name=="BBS 63-910-28","BBS 63",
                           ifelse(test$Name=="BBS 82-903-39","BBS 82",
                                  ifelse(test$Name=="BBS 88-900-29","BBS 88",
                                         ifelse(test$Name=="Coweeta - RK","Cow RK",
                                                ifelse(test$Name=="Georgetown","Georgt",
                                                       ifelse(test$Name=="Coweeta - BB","Cow BB",
                                                              ifelse(test$Name=="Coweeta - BS","Cow BS",
                                                                     ifelse(test$Name=="Sault College","Sault",
                                                                            ifelse(test$Name=="Roxbury Park","RoxPar",
                                                                                   ifelse(test$Name=="NC Botanical Garden","NCBG",
                                                                                          ifelse(test$Name=="Prairie Ridge Ecostation","PrairEco",
                                                                                                 ifelse(test$Name=="Great Smoky Mountains Institute at Tremont","Tremont",
                                                                                                        ifelse(test$Name=="Stage Nature Center","Stage",
                                                                                                               ifelse(test$Name=="Mataniuck State Park","Matan",
                                                                                                                      ifelse(test$Name=="Environmental Sciences Magnet School","ESMS",
                                                                                                                             ifelse(test$Name=="UNC Chapel Hill Campus","UNCCHC",
                                                                                                                                    ifelse(test$Name=="Example Site","Examp",
                                                                                                                                           ifelse(test$Name=="Marin Municipal Water District","Marin",
                                                                                                                                                  ifelse(test$Name=="Museum of American Bird Art","MoABA",
                                                                                                                                                         ifelse(test$Name=="Mass Audubon's Broadmoor Wildlife Sanctuary","Broadmoor",
                                                                                                                                                              ifelse(test$Name=="Azalea Repository (TNCA)","Azal Repo",
                                                                                                                                                                               ifelse(test$Name=="CawCaw Interpretive Center","CawCaw","NA")))))))))))))))))))))))


test$code<-site_code
site<-ggplot(test, aes(fct_reorder(code, order), pct, fill = trait)) + geom_col(position = "stack")+
  theme(axis.text.x = element_text( color="black", 
                                    size=8, angle=45,vjust=1,hjust=1),
        axis.text.y = element_text( color="black", 
                                    size=7, angle=45))+theme(plot.margin=margin(10,20,50,50))+xlab("Site")+ylab("Percent composition")+guides(fill=FALSE)

site
test_lat<-defense_by_site%>%
  gather(trait,pct,5:8)
test_lat$roundup<-round_any(test_lat$Latitude,.001)

lat<-ggplot(test_lat,aes(x=factor(test_lat$roundup), pct,fill=trait))+geom_col(position="stack",width=.8)+ theme(axis.text.x = element_text( color="black", size=9, angle=45,vjust=1,hjust=1))+xlab("Latitude")+ylab("Percent composition")

lat
grid.arrange(site,lat,nrow=1)

#Phenology plots
#Not sure how to get around just individually plotting each year, thought maybe a for loop might be possible but didn't know how to execute it
# PR<-for(Year in unique(Year)){
#      Phenoplot<-subset(PRpheno,Year==Year)
#      ggplot(PRpheno,aes(julianday))+
#      geom_line(aes(y=pct_defend,colour="Defended"))+
#      geom_line(aes(y=pct_undefend,colour="Undefended"))+
#      geom_point(aes(y=pct_undefend))+
#      geom_point(aes(y=pct_defend))+
#      xlab("Julian Day")+
#      ylab("Percentage of Surveys")
#  }

PR2016_filter<-PRpheno%>%
  filter(Year=="2016")

PR2016<-ggplot(PR2016_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("Prairie Ridge 2016 Phenology")


NC2016_filter<-NCBGpheno%>%
  filter(Year=="2016")

NC2016<-ggplot(NC2016_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("NC Botanical Garden 2016 Phenology")

PR2017_filter<-PRpheno%>%
  filter(Year=="2017")
PR2017<-ggplot(PR2017_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("Prairie Ridge 2017 Phenology")

NC2017_filter<-NCBGpheno%>%
  filter(Year=="2017")

NC2017<-ggplot(NC2017_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("NC Botanical Garden 2017 Phenology")

PR2018_filter<-PRpheno%>%
  filter(Year=="2018")

PR2018<-ggplot(PR2018_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("Prairie Ridge 2018 Phenology")

NC2018_filter<-NCBGpheno%>%
  filter(Year=="2018")

NC2018<-ggplot(NC2018_filter,aes(julianday))+
  geom_line(aes(y=pct_defend,colour="Defended"))+
  geom_line(aes(y=pct_undefend,colour="Undefended"))+
  geom_point(aes(y=pct_undefend))+
  geom_point(aes(y=pct_defend))+
  xlab("Julian Day")+
  ylab("Percentage of Surveys")+
  ggtitle("NC Botanical Garden 2018 Phenology")

grid.arrange(NC2016,PR2016,NC2017,PR2017,NC2018,PR2018,nrow=3)


