## All iNaturalist records through 2017
## User behavior

library(tidyverse)
library(dbplyr)
library(lubridate)

setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

# Observation-days per year

obs_days_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(year = substr(observed_on, 1, 4)) %>%
  distinct(year, observed_on, user_login, id) %>%
  group_by(year) %>%
  count(observed_on, user_login) %>%
  count(n) %>%
  rename("obs_days" = n,
         "count" = nn)

obs_days_df <- obs_days_db %>%
  collect()

# Number of iNat observations over annual cycle (weekly)
# Number of unique users over annual cycle (weekly)

obs_effort_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, month, jd_wk, observed_on, user_login, id) %>%
  group_by(year, month, jd_wk) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

obs_effort_df <- obs_effort_db %>%
  collect()

# Observations per day

# Observations per day per observer

# Observations per month per observer

# Taxonomic breadth of observations
# PCA for users - iconic_taxon_name

icon_taxa_db <- tbl(con, "inat") %>%
  group_by(user_login, iconic_taxon_name) %>%
  count()

icon_taxa_df <- icon_taxa_db %>%
  collect()

icon_taxa_clean <- icon_taxa_df %>%
  filter(user_login != "", iconic_taxon_name != "") %>%
  group_by(iconic_taxon_name) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs > 20)

taxa <- unique(icon_taxa_clean$iconic_taxon_name)

# Animalia contains non-Insect/arachnid arthropods (millipedes, centipedes), aquatic organisms: shrimp, crabs, corals, sea stars
animalia_db <- tbl(con, "inat") %>%
  filter(iconic_taxon_name == "Animalia") 

animalia_df <- animalia_db %>%
  collect()

# Tendency to submit repeat observations of the same species/taxa in a day, month, year, all time
