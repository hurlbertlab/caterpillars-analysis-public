### iNaturalist database - 2018 and earlier
## Empirical inferences of user type

## Libraries

library(tidyverse)
library(dbplyr)
library(RSQLite)

## Plotting theme

theme_set(theme_classic(base_size = 15))

# Append correct BioArk path
info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\BioArk")

## iNat database

setwd(paste0(bioark, "/HurlbertLab/Databases/iNaturalist/"))
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

#### User Behavior for descriptive MS: Species-accumulation, observation evenness ####

## CDF: proportion of total observations from users of different # observations

total_obs <- sum(user_profiles$n_obs)

user_obs_alltime <- user_profiles %>%
  group_by(user_login) %>%
  summarize(obs = sum(n_obs),
            prop_total_obs = obs/total_obs)

ggplot(user_obs_alltime, aes(x = obs)) + 
  stat_ecdf(geom = "step") + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000)) +
  geom_vline(xintercept = 10, lty = 2) +
  geom_hline(yintercept = 0.75, lty = 2) +
  labs(x = "User's total numebr of observations", y = "Proportion of total users")
ggsave("figs/inaturalist/cdf_users_by_total_obs.pdf")

user_cumsum_plot <- user_obs_alltime %>%
  arrange(obs) %>%
  mutate(cum_prop_obs = cumsum(prop_total_obs))

above_1000 <- filter(user_cumsum_plot, obs > 1000)

above_10000 <- filter(user_cumsum_plot, obs > 10000)

ggplot(data = user_cumsum_plot, aes(x = obs, y = cum_prop_obs)) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000)) +
  geom_ribbon(data = above_1000, aes(x = obs, ymin = 0, ymax = cum_prop_obs), fill = "lightskyblue") +
  geom_line(data = user_cumsum_plot, aes(x = obs, y = cum_prop_obs), cex = 1) +
  geom_hline(yintercept = 0.5, lty = 2) + 
  labs(x = "User's total number of observations", y = "Proportion of iNaturalist observations")
ggsave("figs/inaturalist/cumulative_observations_by_user.pdf")
# 2,661 users above 1000 observations, 500,000+ users all time
nrow(above_1000)/nrow(user_cumsum_plot) # 0.00515

## Median observations per day and observations per month [May-Sept] for each user

inat_user_monthly_medians_db <- tbl(con, "inat") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09") %>%
  group_by(user_login, month) %>%
  summarize(n_obs = n_distinct(id)) 

monthly_medians <- inat_user_monthly_medians_db %>%
  collect() %>%
  group_by(user_login) %>%
  summarize(med_obs = median(n_obs))

inat_user_daily_medians_db <- tbl(con, "inat") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  filter(month == "05" | month == "06" | month == "07" | month == "08" | month == "09") %>%
  group_by(user_login, observed_on) %>%
  summarize(n_obs = n_distinct(id))

daily_medians <- inat_user_daily_medians_db %>%
  collect() %>%
  group_by(user_login) %>%
  summarize(med_obs = median(n_obs))

## Shannon equitability



## # spp vs # observations

user_profs_2018 <- user_profiles %>%
  filter(year == 2018, n_spp > 0)

ggplot(user_profs_2018, aes(x = n_obs, y = n_spp)) + geom_point(alpha = 0.1) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) +
  scale_y_log10() +
  labs(x = "User's total number of observations", y = "Number of species")
ggsave("figs/inaturalist/nSpp_vs_nObs.pdf")
## Who is the dot near 10000 observations with 2 spp? kererucount - all NZ pigeons 
## Some w/ tons of obs and very few species are clearly organizations that have been deleted by iNat (e.g. palmettomiddleschool)
View(filter(user_profs_2018, n_obs > 1000))

## For users > 20 obs, mean # species for each # observations binned, on a log scale

above_20 <- user_profiles %>%
  filter(n_obs > 20, year == 2018) %>%
  mutate(obs_bin = ceiling(log(n_obs)),
         obs_bin_val = exp(obs_bin)) %>%
  group_by(obs_bin_val) %>%
  summarize(mean_spp = mean(n_spp))

ggplot(above_20, aes(x = obs_bin_val, y = mean_spp)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000)) +
  labs(x = "Number of observations", y = "Mean number of species")
ggsave("figs/inaturalist/mean_spp_per_user_effort_bin.pdf")
