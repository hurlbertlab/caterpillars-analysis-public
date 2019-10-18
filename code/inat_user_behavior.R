## All iNaturalist records through 2017
## User behavior

library(tidyverse)
library(dbplyr)
library(lubridate)

setwd("C:/Users/gdicecco/Desktop/data/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db", encoding = "UTF-16")

db_list_tables(con)

# Obersvation-days per year

obs_days_db <- tbl(con, "inat") %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         Year = y(Date)) %>%
  distinct(Date, user_login, id) %>%
  group_by(Year) %>%
  count(Date, user_login) %>%
  count(n) %>%
  rename("obs_days" = n,
         "count" = nn)

obs_days_df <- obs_days_db %>%
  collect()

# Observations per day


# Taxonomic breadth of observations
# PCA for users - iconic_taxon_name

icon_taxa_db <- tbl(con, "inat") %>%
  group_by(user_login, iconic_taxon_name) %>%
  count()

icon_taxa_df <- icon_taxa_db %>%
  collect()

icon_taxa_clean <- icon_taxa_df %>%
  filter(user_login != "", iconic_taxon_name != "")

# Tendency to submit repeat observations of the same species in a day, month, year, all time

# Number of iNat observations over annual cycle

# Number of unique users over annual cycle

# Observations per day per observer

# Observations per month per observer