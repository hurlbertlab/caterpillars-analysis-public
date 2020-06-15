#### Compare June insect observations in iNaturalist databases

library(tidyverse)
library(dbplyr)
library(RSQLite)

## Get June arthropod records from all iNaturalist database to control for sampling effort iNat

# For 2019
# setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/inat_thru_2019")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inat_2019.db")

db_list_tables(con)

inat_insects_db <- tbl(con, "inat") %>%
  dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7))

inat_insects_df <- inat_insects_db %>%
  collect()

DBI::dbDisconnect(con, "inat")

inat_2019_june <- inat_insects_df %>%
  filter(year == "2019", month == "06")

length(unique(inat_2019_june$id))
# 75,891 observations

# For earlier years

# setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

inat_insects_early_db <- tbl(con, "inat") %>%
  dplyr::select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  filter(iconic_taxon_name == "Insecta") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7))

inat_insects_early_df <- inat_insects_early_db %>%
  collect()

inat_2018_june <- inat_insects_early_df %>%
  filter(year == "2018", month == "06")

length(unique(inat_2018_june$id))
# 172,043 observations

