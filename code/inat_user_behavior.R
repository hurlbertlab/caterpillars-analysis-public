## All iNaturalist records through 2017
## User behavior

library(tidyverse)
library(dbplyr)

setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "iNaturalist_s.db")

db_list_tables(con)

# For below, set wd to caterpillars-analysis-public git repo
theme_set(theme_classic(base_size = 15))

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

obs_days_plot <- obs_days_df %>%
  filter(year < 2018) %>%
  group_by(year) %>%
  mutate(total_obs_days = sum(count))

pdf("figs/inaturalist/observer-days-per-year.pdf")
for(y in unique(obs_days_plot$year)) {
  df <- filter(obs_days_plot, year == y)
  plot <- ggplot(df, aes(x = obs_days, y = count)) + geom_col() + scale_y_log10() + 
    labs(x = "Observer-days", y = "Count", title = y) +
    annotate(geom = "text", x = max(df$obs_days)-0.2*max(df$obs_days), y = max(df$count), label = paste0("Total obs-days \n", unique(df$total_obs_days)))
  print(plot)
}
dev.off()

# Number of iNat observations over annual cycle (weekly)
# Number of unique users over annual cycle (weekly)

obs_effort_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, jd_wk, observed_on, user_login, id) %>%
  group_by(year, jd_wk) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

obs_effort_df <- obs_effort_db %>%
  collect()

obs_effort_plot <- obs_effort_df %>%
  na.omit() %>%
  filter(year < 2018) %>%
  group_by(year) %>%
  mutate(jd_wk = jd_wk - min(jd_wk))

pdf("figs/inaturalist/observations-per-week.pdf")
for(y in unique(obs_effort_plot$year)) {
  df <- filter(obs_effort_plot, year == y)
  n_ob <- sum(df$n_obs)
  plot <- ggplot(df, aes(x = jd_wk, y = n_obs)) + geom_col(color = "white") + 
    labs(x = "Julian day", y = "Number of observations", title = y) +
    annotate(geom = "text", x = max(df$jd_wk)-0.2*max(df$jd_wk), y = max(df$n_obs), label = paste0("Total observations \n", n_ob))
  print(plot)
}
dev.off()

pdf("figs/inaturalist/users-per-week.pdf")
for(y in unique(obs_effort_plot$year)) {
  df <- filter(obs_effort_plot, year == y)
  plot <- ggplot(df, aes(x = jd_wk, y = n_users)) + geom_col(color = "white") +
    labs(x = "Julian day", y = "Number of unique users", title = y)
  print(plot)
}
dev.off()

# Observations per day per observer

obs_freq_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, month, observed_on, user_login, id) %>%
  group_by(year, month, observed_on, user_login) %>%
  summarize(n_obs = n_distinct(id))

obs_freq_df <- obs_freq_db %>%
  collect()

# Users under 1000 obs
ggplot(filter(obs_freq_df, n_obs < 1000), aes(x = n_obs)) + geom_histogram(bins = 100) + scale_y_log10() +
  labs(x = "Number of observations per day per observer", y = "Count", title = "Under 1000 observations per day")
ggsave("figs/inaturalist/obs-per-day-per-user.pdf")

# Users over 1000 obs
ggplot(filter(obs_freq_df, n_obs >= 1000), aes(x = n_obs)) + 
  geom_histogram(bins = 100) + scale_y_continuous(breaks = c(0,2,4,6,8)) +
  labs(x = "Number of observations per day per observer", y = "Count", title = "Over 1000 observations per day")
ggsave("figs/inaturalist/obs-per-day-per-user-super.pdf")

# Observations per month per observer

obs_freq_month <- obs_freq_df %>%
  group_by(year, month, user_login) %>%
  summarize(n_obs = sum(n_obs))

# Users under 1000 obs
ggplot(filter(obs_freq_month, n_obs < 1000), aes(x = n_obs)) + geom_histogram(bins = 100) +
  labs(x = "Number of observations per month per observer", y = "Count", title = "Under 1000 observations per month")
ggsave("figs/inaturalist/obs-per-month-per-user.pdf")

# Users over 1000 obs
ggplot(filter(obs_freq_month, n_obs >= 1000), aes(x = n_obs)) + 
  geom_histogram(bins = 100) + 
  labs(x = "Number of observations per month per observer", y = "Count", title = "Over 1000 observations per month")
ggsave("figs/inaturalist/obs-per-month-per-user-super.pdf")

# Taxonomic breadth of observations, all time
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
  filter(n_obs > 20) # number of observations per taxa

taxa <- unique(icon_taxa_clean$iconic_taxon_name)

taxa_obs <- icon_taxa_clean %>%
  group_by(iconic_taxon_name) %>%
  summarize(total_obs = sum(n),
            total_users = n_distinct(user_login))

ggplot(taxa_obs, aes(x = iconic_taxon_name, y = total_obs)) + geom_col() +
  labs(x = "", y = "Total observations")
ggsave("figs/inaturalist/taxa_total_obs.pdf")

ggplot(taxa_obs, aes(x = iconic_taxon_name, y = total_users)) + geom_col() +
  labs(x = "", y = "Total users")
ggsave("figs/inaturalist/taxa_total_users.pdf")

# restructure long to wide
icon_taxa_wide <- icon_taxa_clean %>%
  select(user_login, iconic_taxon_name, n) %>%
  spread(key = iconic_taxon_name, value = n) 

icon_taxa_wide[is.na(icon_taxa_wide)] <- 0

# PCA
taxa_pca <- prcomp(icon_taxa_wide[, -1], center = T, scale = T)

tiff("figs/inaturalist/taxa_pca_biplot.tiff", height = 6, width = 9)
biplot(taxa_pca)
dev.off()

# Animalia contains non-Insect/arachnid arthropods (millipedes, centipedes), aquatic organisms: shrimp, crabs, corals, sea stars
animalia_db <- tbl(con, "inat") %>%
  filter(iconic_taxon_name == "Animalia") 

animalia_df <- animalia_db %>%
  collect()

# Tendency to submit repeat observations of the same species in a day, month, year

taxa_freq_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id, scientific_name) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  group_by(year, month, observed_on, user_login, scientific_name) %>%
  summarize(n_obs = n_distinct(id))

taxa_freq_df <- taxa_freq_db %>%
  collect()

quantile(taxa_freq_df$n_obs)
# 0 - 1, 25 - 1, 50 - 1, 75 - 1, 100 - 1520

# same month repeat observations of species
taxa_freq_month <- taxa_freq_df %>%
  group_by(year, month, user_login, scientific_name) %>%
  summarize(n_observations = sum(n_obs))

quantile(taxa_freq_month$n_observations)
# 0 - 1, 25 - 1, 50 - 1, 75 - 1, 100 - 7220

# same year repeat observations of species
taxa_freq_year <- taxa_freq_df %>%
  group_by(year, user_login, scientific_name) %>%
  summarize(n_observations = sum(n_obs))

quantile(taxa_freq_year$n_observations)
# 0 - 1, 25 - 1, 50 - 1, 75 - 1, 100 - 8820

