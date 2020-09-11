### iNaturalist descriptive MS
### Data cleaning: through 2019

#### Libraries ####

library(tidyverse)
library(RSQLite)
library(dbplyr)

#### Read in datasets ####

# Append correct BioArk path

info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\BioArk")

setwd(paste0(bioark, "/HurlbertLab/Databases/iNaturalist/"))

# Through 2018 db

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

# 2019 RDS

setwd("./inat_2019_rds")

files <- list.files()[grepl("_c", list.files())]

#### Figure 1: iNat Growth ####

# number of users per year, number of observations per year

### Data through 2018

inat_2018_fig1_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, jd_wk, observed_on, user_login, id) %>%
  group_by(year) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

inat_2018_fig1 <- inat_2018_fig1_db %>%
  collect()

### Data through 2019

inat_2019_fig1 <- data.frame(year = c(), month = c(), n_obs = c())
users_2019 <- c()

Sys.time()
for(f in files) {
  df <- readRDS(f)
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(year == 2019) %>%
    group_by(year, month) %>%
    summarize(n_obs = n_distinct(id))
  
  inat_2019_fig1 <- bind_rows(inat_2019_fig1, res)
  
  users_2019 <- c(users_2019, unique(df$user.login))
  
  print(f)
  print(Sys.time())
}

inat_2019_fig1_df <- inat_2019_fig1 %>%
  group_by(year) %>%
  summarize(n_obs = sum(n_obs))

inat_2019_fig1_df$n_users <- length(unique(users_2019))

# Combine thru 2018 & 2019

inat_fig1 <- inat_2018_fig1 %>%
  filter(year < 2019) %>%
  bind_rows(inat_2019_fig1_df)

# Write into repo data folder
# write.csv(inat_fig1, "data/inat_thru_2019_annual_growth.csv", row.names = F)

#### Figure 2: iNat spatial, temporal, taxonomic biases ####

# Unique lat-lon of all observations

### Thru 2018

inat_sites_db <- tbl(con, "inat") %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  dplyr::select(longitude, latitude) %>%
  distinct()

inat_sites_df <- inat_sites_db %>%
  collect()

inat_sites_sf <- inat_sites_df %>%
  mutate(lat = latitude,
         lon = longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs)
# st_write(inat_sites_sf, "data/inat_northam_site_coords.shp")

inat_sites_2018_db <- tbl(con, "inat") %>%
  filter(!is.na(longitude) | !is.na(latitude)) %>%
  dplyr::select(longitude, latitude) %>%
  distinct()

inat_sites_2018_df <- inat_sites_2018_db %>%
  collect()

### 2019

inat_2019_sites <- data.frame(latitude = c(), longitude = c())

Sys.time()
for(f in files) {
  df <- readRDS(f)
  
  res <- df %>%
    filter(!is.na(longitude) | !is.na(latitude)) %>%
    dplyr::select(longitude, latitude) %>%
    distinct()
  
  inat_2019_sites <- bind_rows(inat_2019_sites, res)
  
  print(f)
  print(Sys.time())
}

### Combine thru 2019

inat_thru_2019_sites <- inat_2019_sites %>%
  distinct() %>%
  mutate_all(~as.numeric(.)) %>%
  bind_rows(inat_sites_2018_df)
# write.csv(inat_thru_2019_sites, paste0(repo, "/data/inat_thru_2019_sites.csv"), row.names = F)
# on BioArk b/c too big for git

# Spp per class in whole dataset

### Thru 2018

### 2019

inat_2019_spp <- data.frame(class = c(), species = c())

for(f in files) {
  df <- readRDS(f)
  
  res <- df %>%
    filter(taxon.rank == "species") %>%
    dplyr::select(taxon.name, class) %>%
    distinct()
  
  inat_2019_spp <- bind_rows(inat_2019_spp, res)

  print(f)
}

inat_2019_spp_list <- inat_2019_spp %>%
  distinct()
# write.csv(inat_2019_spp_list, paste0(repo, "/data/inat_2019_spp.csv"), row.names = F)

# Observation phenology in example year - observations grouped by week for 2018

### 2018

obs_effort_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  filter(year == "2018") %>%
  distinct(year, jd_wk, observed_on, user_login, id) %>%
  group_by(year, jd_wk) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

obs_effort_df <- obs_effort_db %>%
  collect()

obs_effort_2018 <- obs_effort_df %>%
  filter(!is.na(jd_wk)) %>%
  mutate(jd_wk = jd_wk - min(jd_wk))

# write.csv(obs_effort_2018, paste0(repo, "/data/inat_2018_annual_user_pheno.csv"), row.names = F)

#### Figures 3, 4 & 5: User behavior ####

# For each user: number of observations per class, number of observations per insect order, 
# total # insect species, total # insect observations 
# total # species, total # observations
# May-Sep obs frequency: median # dates per month
# May-Sep obs intensity: median obs per day
