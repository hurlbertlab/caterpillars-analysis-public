### iNaturalist database - 2018 and earlier
## Empirical inferences of user type

## Libraries

library(tidyverse)
library(dbplyr)
library(RSQLite)

## Plotting theme

theme_set(theme_classic(base_size = 15))

## iNat database

setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

## Group by users, year: # days, # spp, # observations

inat_user_profs_db <- tbl(con, "inat") %>%
  mutate(year = substr(observed_on, 1, 4)) %>%
  group_by(year, user_login) %>%
  summarize(n_days = n_distinct(observed_on),
            n_spp = n_distinct(taxon_id),
            n_obs = n_distinct(id))

user_profiles <- inat_user_profs_db %>%
  collect()
# write.csv(user_profiles, "data/inat_annual_user_profiles.csv", row.names = F)

user_profiles <- read.csv("data/inat_annual_user_profiles.csv", stringsAsFactors = F)

## Obs per day, spp per day, spp per obs

user_rates <- user_profiles %>%
  filter(year < 2019, n_spp > 0) %>%
  mutate(obsperday = n_obs/n_days,
         sppperday = n_spp/n_days,
         sppperobs = n_spp/n_obs)

# Taxon ID/observations calculated by user/year, histogram
ggplot(user_rates, aes(x = sppperobs)) + geom_histogram(col = "white", bins = 20) +
  labs(x = "Annual taxa/observation", y = "Count")
# ggsave("figs/inaturalist/user_taxa_per_obs.pdf")

## Distinguishing type 1 and 2 observers: 
## Type 1: Records every individual of every species
## Type 2: Records every species, not every individual

# One off, type1, type 2 sum to one
user_freq <- user_rates %>%
  group_by(year) %>%
  summarize(n_users = n_distinct(user_login),
            pct_one_off = n_distinct(user_login[n_obs == 1])/n_users,
            pct_type2 = n_distinct(user_login[sppperobs == 1 & n_obs > 1])/n_users,
            pct_type1 = n_distinct(user_login[sppperobs < 1])/n_users,
            pct_strict_type1 = n_distinct(user_login[sppperobs < 0.8])/n_users) %>% # out of 5 observations, one repeat species
  ungroup()

colors <- c("One-off" = "darkgray", "Type 1" = "springgreen", "Type 1 (strict)" = "springgreen4", "Type 2" = "skyblue")

ggplot(user_freq, aes(x = as.numeric(year))) + 
  ylim(0,0.6) +
  geom_line(aes(y = pct_one_off, col = "One-off"), cex = 1) +
  geom_line(aes(y = pct_type2, col = "Type 2"), cex = 1) +
  geom_line(aes(y = pct_type1, col = "Type 1"), cex = 1) +
  geom_line(aes(y = pct_strict_type1, col = "Type 1 (strict)"), cex = 1) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(x = "Year", y = "Proportion of users", color = "Observer type") + 
  scale_color_manual(values = colors) + theme(legend.position = c(0.8, 0.8))
# ggsave("figs/inaturalist/user_types_annual.pdf")

## Out of just repeat users (within a year)
# How is obs per day related to spp per obs in 2018

repeat_users <- user_rates %>%
   filter(n_obs >= 10, year == 2018)

cor(repeat_users[, 6:8])

#Prevalence of Type 1 observer as effort increases

type1_users <- user_rates %>%
  filter(year == 2018, n_obs > 1) %>%
  mutate(obs_bin = round(n_obs, -1)) %>%
  group_by(obs_bin) %>%
  nest() %>%
  mutate(n_users = map_dbl(data, ~length(unique(.$user_login))),
    pct_type1 = map_dbl(data, ~{
    df <- . 
    users <- length(unique(df$user_login))
    type1 <- length(unique(df$user_login[df$sppperobs < 0.8]))
    
    type1/users
    
  })) %>%
  filter(n_users > 100) %>%
  select(-data)

ggplot(type1_users, aes(x = obs_bin, y = pct_type1)) + geom_point() + 
  geom_line(cex = 1) +
  labs(x = "Observations in 2018", y = "Proportion of Type 1 observers")
ggsave("figs/inaturalist/type1_vs_effort.pdf")
