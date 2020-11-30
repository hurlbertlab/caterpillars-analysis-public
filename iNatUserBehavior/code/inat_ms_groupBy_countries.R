# load packages

library(tidyverse)
library(cowplot)
library(sf)

df1 <- readRDS(file = "iNatUserBehavior/bigdata/inat_stjoin1.rds")

inat_groupedBy_countries <- df1 %>% 
  group_by(NAME) %>% 
  summarise(count = n())

saveRDS(inat_groupedBy_countries, file = "iNatUserBehavior/bigdata/inat_records_by_countries1.rds")


df2 <- readRDS(file = "iNatUserBehavior/bigdata/inat_stjoin2.rds")

inat_groupedBy_countries2 <- df2 %>% 
  group_by(NAME) %>% 
  summarise(count = n())

saveRDS(inat_groupedBy_countries2, file = "iNatUserBehavior/bigdata/inat_records_by_countries2.rds")

df3 <- readRDS(file = "iNatUserBehavior/bigdata/inat_stjoin3.rds")

inat_groupedBy_countries3 <- df3 %>% 
  group_by(NAME) %>% 
  summarise(count = n())

saveRDS(inat_groupedBy_countries3, file = "iNatUserBehavior/bigdata/inat_records_by_countries3.rds")
