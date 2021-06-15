### iNaturalist descriptive MS
### Data cleaning: through 2019

#### Libraries ####

library(tidyverse)
library(RSQLite)
library(dbplyr)
library(sf)
library(maptools)

#### Read in datasets ####

repo <- "/Users/gracedicecco/git/caterpillars-analysis-public/iNatUserBehavior/"

# Append correct BioArk path

info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\ad.unc.edu/bio")

setwd(paste0(bioark, "/HurlbertLab/Databases/iNaturalist/"))
setwd("C:/Users/gdicecco/Desktop/data/")

# Through 2018 db

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

# 2019 RDS

setwd(paste0(bioark, "/HurlbertLab/Databases/iNaturalist/inat_thru_2019_taxon_info_rds"))

files <- list.files()[grepl("2019", list.files())]

# All RDS through 2019 with taxon info

files_all <- list.files()[!grepl(".csv", list.files())]

#### Figure 1: iNat Growth ####

# number of users per year, number of observations per year

### Data through 2018

inat_2018_fig1_db <- tbl(con, "inat") %>%
  select(scientific_name, quality_grade, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, jd_wk, observed_on, user_login, id, quality_grade) %>%
  group_by(year) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login),
            n_obs_rg = n_distinct(id[quality_grade == "research"]))

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
    summarize(n_obs = n_distinct(id),
              n_obs_rg = n_distinct(id[quality_grade == "research"]))
  
  inat_2019_fig1 <- bind_rows(inat_2019_fig1, res)
  
  users_2019 <- c(users_2019, unique(df$user.login))
  
  print(f)
  print(Sys.time())
}

inat_2019_fig1_df <- inat_2019_fig1 %>%
  group_by(year) %>%
  summarize(n_obs = sum(n_obs),
            n_obs_rg = sum(n_obs_rg))

inat_2019_fig1_df$n_users <- length(unique(users_2019))

# Combine thru 2018 & 2019

inat_fig1 <- inat_2018_fig1 %>%
  filter(year < 2019) %>%
  bind_rows(inat_2019_fig1_df)

# Write into repo data folder
# write.csv(inat_fig1, "iNatUserBehavior/data/inat_thru_2019_annual_growth.csv", row.names = F)

inat_fig1$prop_rg <- inat_fig1$n_obs_rg/inat_fig1$n_obs

ggplot(inat_fig1, aes(x = year, y = prop_rg)) + geom_point() + theme_classic(base_size = 15) + 
  labs(x = "Year", y = "Proportion of observations research grade")
ggsave("iNatUserBehavior/figs/figs1_propRG.pdf")

#### Figure 1: iNat spatial, temporal, taxonomic biases ####

# Unique lat-lon of all observations

### Thru 2019

inat_2019_sites <- data.frame(latitude = c(), longitude = c())

no_latlon <- c()

Sys.time()
for(f in files_all) {
  df <- readRDS(f)
  
  na <- df %>%
    filter(is.na(longitude) | is.na(latitude))
  
  no_na <- df %>%
    filter(!is.na(longitude) | !is.na(latitude))
  
  res <- no_na %>%
    dplyr::select(longitude, latitude) %>%
    distinct()
  
  inat_2019_sites <- bind_rows(inat_2019_sites, res)
  
  no_latlon <- c(no_latlon, (nrow(na)/(nrow(na) + nrow(no_na))))
  
  print(f)
  print(Sys.time())
}

### Combine thru 2019

inat_thru_2019_sites <- inat_2019_sites %>%
  distinct() %>%
  mutate_all(~as.numeric(.))

# write.csv(inat_thru_2019_sites, "inat_thru_2019_sites.csv"), row.names = F)
# on BioArk b/c too big for git

### Thru 2019 N. Am sites for landcover extraction

inat_thru_2019_nam <- inat_thru_2019_sites %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  mutate(lat = latitude,
         lon = longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))
write.csv(inat_thru_2019_nam, "/Volumes/hurlbertlab/Databases/iNaturalist/inat_northam_site_coords.csv", row.names = F)

## Sites from casual observers for landcover extraction

inat_2019_sites <- data.frame(latitude = c(), longitude = c())

no_latlon <- c()

Sys.time()
for(f in files_all) {
  df <- readRDS(f)
  
  na <- df %>%
    filter(is.na(longitude) | is.na(latitude))
  
  no_na <- df %>%
    filter(!is.na(longitude) | !is.na(latitude))
  
  res <- no_na %>%
    dplyr::select(user.login, id, longitude, latitude) %>%
    distinct()
  
  inat_2019_sites <- bind_rows(inat_2019_sites, res)
  
  no_latlon <- c(no_latlon, (nrow(na)/(nrow(na) + nrow(no_na))))
  
  print(f)
  print(Sys.time())
}

# Filter to casual observers
inat_thru_2019_nam_casual <- inat_2019_sites %>%
  filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
  group_by(user.login) %>%
  mutate(n_obs = n_distinct(id)) %>%
  filter(n_obs == 1) %>%
  dplyr::select(longitude, latitude) %>%
  distinct() %>%
  mutate_all(~as.numeric(.)) %>%
  mutate(lat = latitude,
         lon = longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))
write.csv(inat_thru_2019_nam, "/Volumes/hurlbertlab/Databases/iNaturalist/inat_northam_site_coords_casual.csv", row.names = F)


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

# Weekend effect

weekend_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  filter(year == "2018") %>%
  group_by(year, observed_on, jday) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

weekend_df <- weekend_db %>%
  collect()

weekend_days <- weekend_df %>%
  filter(!is.na(jday), !grepl(":", observed_on)) %>%
  filter(observed_on != "2018-04-27", observed_on != "2018-04-28", observed_on != "2018-04-29", observed_on != "2018-04-30") %>% # CNC2018
  mutate(date = as.Date(observed_on, format = "%Y-%m-%d"),
         weekday = weekdays(date))
write.csv(weekend_days, "data/day_of_week_obs_users.csv", row.names = F)

#### Figures 3, 4 & 5: User behavior ####

# For each user:  
# total # insect species, total # insect observations
# total # species, total # observations

# thru 2019

user_profs_folder <- paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/")

for(f in files_all) {
  df <- readRDS(f)
  
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(year, month, user.login) %>%
    mutate(n_obs = n_distinct(id),
           n_obs_insect = n_distinct(id[class == "Insecta"])) %>%
    dplyr::select(year, month, user.login, n_obs, n_obs_insect, class, order, taxon_name) %>%
    distinct()
  
  write.csv(res, paste0(user_profs_folder, f, "_user_profs.csv"), row.names = F)
  
  print(f)
}

user_profs <- list.files(user_profs_folder)[grepl("rds_user_profs.csv", list.files(user_profs_folder))]

inat_user_profs_thru2019 <- data.frame(filename = user_profs) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    read.csv(paste0(user_profs_folder, f))
  }))

inat_users_total_obs <- inat_user_profs_thru2019  %>%
  unnest(cols = c("df")) %>%
  ungroup() %>%
  dplyr::select(year, month, user.login, n_obs, n_obs_insect) %>%
  distinct() %>%
  group_by(user.login) %>%
  summarize(total_obs = sum(n_obs),
            total_obs_insect = sum(n_obs_insect))
# write.csv(inat_users_total_obs, paste0(repo, "data/inat_thru2019_user_obs.csv"), row.names = F)

## Run this chunk on longleaf
# write_rds(inat_user_profs_thru2019, "/Users/gracedicecco/Desktop/inat_user_profiles.rds")

inat_users_spp <- inat_user_profs_thru2019 %>%
  unnest(cols = c("df")) %>%
  ungroup() %>%
  dplyr::select(user.login, class, order, taxon_name) %>%
  distinct() %>%
  group_by(user.login) %>%
  summarize(n_class = n_distinct(class),
            n_spp = n_distinct(taxon_name),
            n_order_insect = n_distinct(order[class == "Insecta"]),
            n_spp_insect = n_distinct(taxon_name[class == "Insecta"]))
# write.csv(inat_users_spp, paste0(repo, "data/inat_thru2019_user_spp.csv"), row.names = F)

# For each user: number of observations per class, number of observations per insect order

for(f in files_all) {
  df <- readRDS(f)
  
  res_class <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(year, month, user.login, class) %>%
    summarize(n_obs = n_distinct(id))
  
  write.csv(res_class, paste0(user_profs_folder, f, "_user_profs_class.csv"), row.names = F)
  
  res_orders <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(taxon.rank == "species", !is.na(taxon_name), class == "Insecta") %>%
    group_by(year, month, user.login, order) %>%
    summarize(n_obs = n_distinct(id))
  
  write.csv(res_orders, paste0(user_profs_folder, f, "_user_profs_orders.csv"), row.names = F)
  
  print(f)
}

orders_files <- list.files(user_profs_folder)[grepl("profs_orders", list.files(user_profs_folder))]

classes_files <- list.files(user_profs_folder)[grepl("profs_class", list.files(user_profs_folder))]

inat_user_obs_orders <- data.frame(filename = orders_files) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    df <- read.csv(paste0(user_profs_folder, f))
    
    df
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(df))

inat_user_obs_orders_thru2019 <- inat_user_obs_orders %>%
  group_by(user.login, order) %>%
  summarize(total_obs = sum(n_obs))
# write.csv(inat_user_obs_orders_thru2019, paste0(user_profs_folder, "inat_user_obs_insecta_orders_thru2019.csv"), row.names = F)

inat_user_obs_classes <- data.frame(filename = classes_files) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    df <- read.csv(paste0(user_profs_folder, f))
    
    df
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(df))

inat_user_obs_classes_thru2019 <- inat_user_obs_classes %>%
  group_by(user.login, class) %>%
  summarize(total_obs = sum(n_obs))
write.csv(inat_user_obs_classes_thru2019, paste0(user_profs_folder, "inat_user_obs_classes_thru2019.csv"), row.names = F)

# User evenness: classes, insect orders

n_classes <- length(unique(inat_user_obs_classes_thru2019$class))

# 218 classes
user_even_class <- inat_user_obs_classes_thru2019 %>%
  group_by(user.login) %>%
  summarize(all_obs = sum(total_obs),
            shannonH = -sum((total_obs/all_obs)*log(total_obs/all_obs), na.rm = T),
            shannonE = shannonH/log(n_classes))
# write.csv(user_even_class, paste0(repo, "data/inat_user_evenness_class.csv"), row.names = F)

n_orders <- length(unique(inat_user_obs_orders_thru2019$order))

# 27 orders
user_even_order <- inat_user_obs_orders_thru2019 %>%
  group_by(user.login) %>%
  summarize(all_obs = sum(total_obs),
            shannonH = -sum((total_obs/all_obs)*log(total_obs/all_obs), na.rm = T),
            shannonE = shannonH/log(n_orders))
# write.csv(user_even_order, paste0(repo, "data/inat_user_evenness_orders.csv"), row.names = F)

# Expected evenness
# Number of obserations per class/order, number of species per class/order in 2019 dataset

inat_species <- data.frame(year = c(), month = c(), class = c(), order = c(), taxon_name = c())

Sys.time()
for(f in files_all) {
  df <- readRDS(f)
  
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(year, month, class, order, taxon_name) %>%
    summarize(n_obs = n_distinct(id))
  
  inat_species <- bind_rows(inat_species, res)
  
  print(f)
  print(Sys.time())
}

inat_species_thru2019 <- inat_species %>%
  group_by(class, order, taxon_name) %>%
  summarize(total_obs = sum(n_obs))
# write.csv(inat_species_thru2019, paste0(repo, "data/inat_taxon_info_thru2019.csv"), row.names = F)

# Table of frequency/intensity measures
# May-Sep obs frequency: median # dates per year
# May-Sep obs intensity: median obs per day

# thru 2019

for(f in files_all) {
  df <- readRDS(f)
  
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7),
           day = substr(observed_on_details.date, 9, 10)) %>%
    filter(month %in% c("05", "06", "07", "08", "09")) %>%
    group_by(user.login, year, month, day) %>%
    summarize(n_obs = n_distinct(id))
  
  if(nrow(res) > 0) {
    write.csv(res, paste0(user_profs_folder, f, "_user_freq.csv"), row.names = F)
  }
  
  print(f)
}

user_freq_files <- list.files(user_profs_folder)[grepl("user_freq", list.files(user_profs_folder))]

user_freq <- data.frame(filename = user_freq_files) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    df <- read.csv(paste0(user_profs_folder, f))
    
    df
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(df)) %>%
  group_by(user.login, year, month, day) %>%
  summarize(total_obs = sum(n_obs))

# Dates per year (users 2nd year on)

user_annual_freq <- user_freq %>%
  group_by(user.login) %>%
  filter(n_distinct(year) > 1) %>%
  group_by(user.login, year) %>%
  summarize(dates = n())

quantile(user_annual_freq$dates, c(0.05, 0.50, 0.95))
# 1, 3, 37

# Obs per day

quantile(user_freq$total_obs, c(0.05, 0.50, 0.95))
# 1, 1, 15

# Obs per user

user_total_obs <- user_freq %>%
  group_by(user.login) %>%
  summarize(obs = sum(total_obs))
quantile(user_total_obs$obs, c(0.05, 0.50, 0.95))
# 1, 3, 64

## % of observations that come from top 10 species by # records
## Casual (accounts with <5 observations) vs. non-casual

# top 10 species
inat_species <- read_csv("iNatUserBehavior/data/inat_taxon_info_thru2019.csv") %>%
  arrange(desc(total_obs)) %>%
  slice(1:10)

# total observations casual/non-casual
user_group <- user_freq %>%
  group_by(user.login) %>%
  summarize(obs = sum(total_obs)) %>%
  mutate(group = case_when(obs < 5 ~ "casual",
         TRUE ~ "non-casual"))  

group_obs <- user_group %>%
  group_by(group) %>%
  summarize(total_obs = sum(obs))

# Observations of 10 most common spp

for(f in files_all) {
  df <- readRDS(f)
  
  common_spp <- df %>%
    filter(taxon_name %in% inat_species$taxon_name) %>%
    group_by(user.login) %>%
    summarize(n_obs = n_distinct(id))
  
  write.csv(common_spp, paste0(user_profs_folder, f, "_user_profs_commonspp.csv"), row.names = F)
  
  print(f)
}

common_spp_files <- list.files(user_profs_folder)[grepl("commonspp", list.files(user_profs_folder))]

users_spp <- data.frame(filename = common_spp_files) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    df <- read.csv(paste0(user_profs_folder, f))
    
    df
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(df)) %>%
  group_by(user.login) %>%
  summarize(total_obs = sum(n_obs))

common_spp_by_grp <- users_spp %>%
  left_join(user_group, by = c("user.login")) %>%
  group_by(group) %>%
  summarize(common_obs = sum(total_obs))

common_spp_by_grp %>%
  left_join(group_obs) %>%
  mutate(prop_common = common_obs/total_obs)
# Casual observers 4%, observers with 5 or more obs - 3%

