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

# For each user:  
# total # insect species, total # insect observations
# total # species, total # observations

# thru 2018

# 2019

user_profs_folder <- paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/")

for(f in files) {
  df <- readRDS(f)
  
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(year == 2019) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(year, month, user.login) %>%
    mutate(n_obs = n_distinct(id),
           n_obs_insect = n_distinct(id[class == "Insecta"])) %>%
    dplyr::select(year, month, user.login, n_obs, n_obs_insect, class, order, taxon_name) %>%
    distinct()
  
  write.csv(res, paste0(user_profs_folder, "data/", f, "_user_profs.csv"), row.names = F)
  
  print(f)
}

user_profs <- list.files(user_profs_folder)[grepl("rds_user_profs.csv", list.files(user_profs_folder))]

inat_user_profs_2019 <- data.frame(filename = user_profs) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = map(filename, ~{
    f <- .
    read.csv(paste0(repo, "data/", f))
  }))

inat_users_total_obs <- inat_user_profs_2019  %>%
  unnest(cols = c("df")) %>%
  ungroup() %>%
  dplyr::select(year, month, user.login, n_obs, n_obs_insect) %>%
  distinct() %>%
  group_by(year, user.login) %>%
  summarize(total_obs = sum(n_obs),
            total_obs_insect = sum(n_obs_insect))
# write.csv(inat_users_total_obs, paste0(repo, "data/inat_2019_user_obs.csv"), row.names = F)

inat_users_spp <- inat_user_profs_2019 %>%
  unnest(cols = c("df")) %>%
  ungroup() %>%
  dplyr::select(user.login, class, order, taxon_name) %>%
  distinct() %>%
  group_by(user.login) %>%
  summarize(n_class = n_distinct(class),
            n_spp = n_distinct(taxon_name),
            n_order_insect = n_distinct(order[class == "Insecta"]),
            n_spp_insect = n_distinct(taxon_name[class == "Insecta"]))
# write.csv(inat_users_spp, paste0(repo, "data/inat_2019_user_spp.csv"), row.names = F)

# For each user: number of observations per class, number of observations per insect order

for(f in files) {
  df <- readRDS(f)
  
  res_class <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(year == 2019) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(year, month, user.login, class) %>%
    summarize(n_obs = n_distinct(id))
  
  write.csv(res_class, paste0(user_profs_folder, f, "_user_profs_class.csv"), row.names = F)
  
  res_orders <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(year == 2019) %>%
    filter(taxon.rank == "species", !is.na(taxon_name), class == "Insecta") %>%
    group_by(year, month, user.login, order) %>%
    summarize(n_obs = n_distinct(id))
  
  write.csv(res_orders, paste0(user_profs_folder, "_user_profs_orders.csv"), row.names = F)
  
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

inat_user_obs_orders_2019 <- inat_user_obs_orders %>%
  group_by(user.login, order) %>%
  summarize(total_obs = sum(n_obs))
# write.csv(inat_user_obs_orders_2019, paste0(user_profs_folder, "inat_user_obs_insecta_orders_2019.csv"), row.names = F)

inat_user_obs_classes <- data.frame(filename = classes_files) %>%
  group_by(filename) %>%
  nest() %>%
  mutate(df = purrr::map(filename, ~{
    f <- .
    df <- read.csv(paste0(repo, "data/", f))
    
    df
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(df))

inat_user_obs_classes_2019 <- inat_user_obs_classes %>%
  group_by(user.login, class) %>%
  summarize(total_obs = sum(n_obs))
# write.csv(inat_user_obs_classes_2019, paste0(user_profs_folder, "inat_user_obs_classes_2019.csv"), row.names = F)

# User evenness: classes, insect orders

n_classes <- length(unique(inat_user_obs_classes_2019$class))

# 158 classes
user_even_class <- inat_user_obs_classes_2019 %>%
  group_by(user.login) %>%
  summarize(all_obs = sum(total_obs),
            shannonH = -sum((total_obs/all_obs)*log(total_obs/all_obs), na.rm = T),
            shannonE = shannonH/log(n_classes))
# write.csv(user_even_class, paste0(repo, "data/inat_user_evenness_class.csv"), row.names = F)

n_orders <- length(unique(inat_user_obs_orders_2019$order))

# 25 orders
user_even_order <- inat_user_obs_orders_2019 %>%
  group_by(user.login) %>%
  summarize(all_obs = sum(total_obs),
            shannonH = -sum((total_obs/all_obs)*log(total_obs/all_obs), na.rm = T),
            shannonE = shannonH/log(n_orders))
# write.csv(user_even_order, paste0(repo, "data/inat_user_evenness_orders.csv"), row.names = F)

# Expected evenness
# Number of obserations per class/order, number of species per class/order in 2019 dataset

inat_species <- data.frame(month = c(), class = c(), order = c(), taxon_name = c())

Sys.time()
for(f in files) {
  df <- readRDS(f)
  
  res <- df %>%
    mutate(year = substr(observed_on_details.date, 1, 4),
           month = substr(observed_on_details.date, 6, 7)) %>%
    filter(year == 2019) %>%
    filter(taxon.rank == "species", !is.na(taxon_name)) %>%
    group_by(month, class, order, taxon_name) %>%
    summarize(n_obs = n_distinct(id))
  
  inat_species <- bind_rows(inat_species, res)
  
  print(f)
  print(Sys.time())
}

inat_species_2019 <- inat_species %>%
  group_by(class, order, taxon_name) %>%
  summarize(total_obs = sum(n_obs))
# write.csv(inat_species_2019, paste0(repo, "data/inat_taxon_info_2019.csv"), row.names = F)


# May-Sep obs frequency: median # dates per month
# May-Sep obs intensity: median obs per day

# thru 2018

# 2019

