## All iNaturalist records through 2017
## User behavior

library(tidyverse)
library(dbplyr)
library(forcats)
library(cowplot)
library(tmap)
library(sf)
library(lubridate)
library(maptools)

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
  filter(year > 2008 & year < 2018) %>%
  group_by(year) %>%
  mutate(total_obs_days = sum(count))

pdf("figs/inaturalist/observer-days-per-year.pdf")
for(y in unique(obs_days_plot$year)) {
  df <- filter(obs_days_plot, year == y)
  plot <- ggplot(df, aes(x = obs_days, y = count + 1)) + geom_col(width = 0.05) + scale_y_log10() + scale_x_log10() +
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
  filter(year > 2008 & year < 2018) %>%
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

# Phenology in observer effort by latitude bin

obs_effort_lat_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  filter(!is.na(latitude)) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7),
         lat_bin = floor(latitude)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(lat_bin, year, jd_wk, observed_on, user_login, id) %>%
  group_by(lat_bin, year, jd_wk) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

obs_effort_lat_df <- obs_effort_lat_db %>%
  collect()

obs_effort_lat_plot <- obs_effort_lat_df %>%
  na.omit() %>%
  filter(year > 2008 & year < 2018) %>%
  group_by(year) %>%
  mutate(jd_wk = jd_wk - min(jd_wk))

obs_effort_plot_2017 <- obs_effort_lat_plot %>%
  filter(year == 2017) %>%
  group_by(lat_bin) %>%
  mutate(n_weeks = n_distinct(jd_wk)) %>%
  filter(lat_bin > 25 & lat_bin < 60)

ggplot(obs_effort_plot_2017, aes(x = jd_wk, y = n_obs)) + geom_col(color = "white") + 
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~lat_bin, scales = "free") +
  labs(x = "Julian week", y = "Number of observations")
ggsave("figs/inaturalist/seasonal_observations_latitude.pdf", height = 12, width = 12, units = "in")

# Yearly growth

obs_year_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id) %>%
  mutate(jday = julianday(observed_on),
         jd_wk = 7*floor(jday/7)) %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  distinct(year, jd_wk, observed_on, user_login, id) %>%
  group_by(year) %>%
  summarize(n_obs = n_distinct(id),
            n_users = n_distinct(user_login))

obs_year_df <- obs_year_db %>%
  collect()

obs_year_df$group <- 1

ggplot(filter(obs_year_df, year < 2019), aes(x = year, y = n_obs, group  = group)) + geom_line(cex = 1) +
  labs(x = "Year", y = "Number of observations")
ggsave("figs/inaturalist/obs-per-year.pdf")

ggplot(filter(obs_year_df, year < 2019), aes(x = year, y = n_users, group = group)) + geom_line(cex = 1) +
  labs(x = "Year", y = "Number of users")
ggsave("figs/inaturalist/users-per-year.pdf")

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

ggplot(obs_freq_df, aes(x = n_obs + 1)) + geom_histogram(bins = 100) + scale_y_log10() + scale_x_log10() +
  labs(x = "Number of observations per day per observer", y = "Count")
ggsave("figs/inaturalist/obs-per-day-per-user.pdf")

# Observations per month per observer

obs_freq_month <- obs_freq_df %>%
  group_by(year, month, user_login) %>%
  summarize(n_obs = sum(n_obs))

ggplot(obs_freq_month, aes(x = n_obs + 1)) + geom_histogram(bins = 100) + scale_y_log10() + scale_x_log10() +
  labs(x = "Number of observations per month per observer", y = "Count", title = "Under 1000 observations per month")
ggsave("figs/inaturalist/obs-per-month-per-user.pdf")

# Taxonomic breadth of observations, all time
# PCA for users - iconic_taxon_name

icon_taxa_db <- tbl(con, "inat") %>%
  group_by(user_login, iconic_taxon_name) %>%
  count()

icon_taxa_df <- icon_taxa_db %>%
  collect()
#516623 users

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
# Proportions of observations instead of raw numbers
icon_taxa_wide <- icon_taxa_clean %>%
  select(user_login, iconic_taxon_name, n) %>%
  group_by(user_login) %>%
  mutate(total_obs = sum(n)) %>%
  filter(total_obs > 20) %>%
  mutate(prop_obs = n/total_obs) %>%
  select(user_login, iconic_taxon_name, prop_obs) %>%
  spread(key = iconic_taxon_name, value = prop_obs)
#71881 users

icon_taxa_wide[is.na(icon_taxa_wide)] <- 0

# PCA
taxa_pca <- prcomp(icon_taxa_wide[, -1], center = T, scale = T)
# PC1 - 15.7 %, PC2 - 12.5 %
# First 4 axes - 49% of variance

png("figs/inaturalist/taxa_pca_biplot.png", height = 6, width = 9, units = "in", res = 72)
biplot(taxa_pca)
dev.off()

# Animalia contains non-Insect/arachnid arthropods (millipedes, centipedes), aquatic organisms: shrimp, crabs, corals, sea stars
animalia_db <- tbl(con, "inat") %>%
  filter(iconic_taxon_name == "Animalia") 

animalia_df <- animalia_db %>%
  collect()

# Super users: broken down by taxa

icon_taxa_super<- icon_taxa_clean %>%
  select(user_login, iconic_taxon_name, n) %>%
  group_by(user_login) %>%
  mutate(total_obs = sum(n)) %>%
  filter(total_obs > 10000)

super_users <- icon_taxa_super %>%
  ungroup() %>%
  select(user_login, total_obs) %>%
  distinct() %>%
  arrange(desc(total_obs)) %>%
  slice(1:30)
# all > 25,900 observations, max = 115000

ggplot(super_users, aes(x = total_obs)) + geom_histogram(col = "white", binwidth = 10000) +
  labs(x = "Count", y = "Total observations")
ggsave("figs/inaturalist/super_users_alltaxa_hist.pdf")

super_users_plot <- icon_taxa_super %>%
  filter(user_login %in% super_users$user_login) %>%
  mutate(prop_obs = n/total_obs) %>%
  mutate(arrange = prop_obs[iconic_taxon_name == "Aves"])

super_users_plot$taxon <- fct_collapse(super_users_plot$iconic_taxon_name, 
                                       Other = c("Reptilia", "Amphibia", "Chromista", "Arachnida", "Mammalia", "Mollusca", "Protozoa", "Animalia"))

ggplot(super_users_plot, aes(x = forcats::fct_reorder(user_login, arrange), y = prop_obs, fill = taxon)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d() +
  labs(y = "Proportion of observations", fill = "Iconic taxon name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank())
ggsave("figs/inaturalist/super_users_taxa.pdf")

# insect super users: geographic scope & temporal scope

add_group_summaries <- function(d, groupingVars, ...) {
  # convert char vector into quosure vector
  # worked out after reading http://dplyr.tidyverse.org/articles/programming.html
  # no idea if this will be stable across rlang/tidyeval versions
  groupingQuos <- lapply(groupingVars, 
                         function(si) { quo(!!as.name(si)) })
  # print(groupingQuos)
  dg <- group_by(d, !!!groupingQuos)
  ds <- summarize(dg, ...)
  # Work around: https://github.com/tidyverse/dplyr/issues/2963
  ds <- ungroup(ds)
  left_join(d, ds, by= groupingVars)
}

super_users_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id, scientific_name) %>%
  filter(iconic_taxon_name == "Arachnida" | iconic_taxon_name == "Insecta") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  add_group_summaries("user_login", 
                      total_obs = n_distinct(id)) %>%
  filter(total_obs > 1000)

super_users_df <- super_users_db %>%
  collect()
# 725 users

super_users_hist <- super_users_df %>%
  ungroup() %>%
  distinct(user_login, total_obs)

ggplot(super_users_hist, aes(x = total_obs)) + geom_histogram(binwidth = 1000, col = "white") +
  labs(x = "Total observations", y = "Count")
ggsave("figs/inaturalist/super_users_insecta_hist.pdf")

super_users_time <- super_users_df %>%
  group_by(user_login) %>%
  summarize(beg = min(as.numeric(year)),
            end = max(as.numeric(year)),
            length = max(as.numeric(year)) - min(as.numeric(year)))

super_users_seasonal <- super_users_df %>%
  filter(year == 2017) %>%
  mutate(date = as.Date(observed_on, format = "%Y-%m-%d"),
         jday = yday(date),
         jd_wk = 7*floor(jday/7)) %>%
  group_by(jd_wk) %>%
  summarize(n_obs = n_distinct(id))

beg <- ggplot(super_users_time, aes(x = beg)) + geom_histogram() +
  labs(x = "First year of observations", y = "Count")

end <- ggplot(super_users_time, aes(x = end)) + geom_histogram() +
  labs(x = "Last year of observations", y = "Count")

time <- ggplot(super_users_time, aes(x = length)) + geom_histogram() +
  labs(x = "Years active", y = "Count")

season <- ggplot(super_users_seasonal, aes(x = jd_wk, y = n_obs)) + geom_col(col = "white") +
  annotate(geom = "text", x = 15, y = max(super_users_seasonal$n_obs), label = "2017", size = 5) +
  labs(y = "Observations", x = "Julian day")

plot_grid(beg, end, time, season, nrow = 2)
ggsave("figs/inaturalist/insect_super_users_temporal.pdf")

data(wrld_simpl)
world <-  wrld_simpl %>% 
  st_as_sf()

super_users_space <- super_users_df %>%
  group_by(user_login) %>%
  distinct(latitude, longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersection(world)

super_users_country <- super_users_space %>%
  group_by(user_login, NAME) %>%
  count()

country_obs <- super_users_country %>%
  group_by(NAME) %>%
  summarize(sum_obs = sum(n)) %>%
  st_set_geometry(NULL)

world_plot <- world %>%
  left_join(country_obs) %>%
  mutate(log_sum = log10(sum_obs))

super_obs_map <- tm_shape(world_plot) + tm_polygons(col = "log_sum", palette = "YlGnBu", title = "Log10(Observations)") + 
  tm_layout(legend.text.size = 1, legend.title.size = 1.25)
tmap_save(super_obs_map, "figs/inaturalist/insect_super_obs_map.pdf")

states <- read_sf("data/maps/ne_50m_admin_1_states_provinces_lakes.shp") %>%
  filter(sr_adm0_a3 == "USA" | sr_adm0_a3 == "CAN") %>%
  filter(name != "Alaska" & name != "Hawaii") %>%
  st_crop(c(xmin = -178, ymin = 18, xmax = -52.65, ymax = 60))

super_users_us <- super_users_space %>%
  filter(NAME == "United States" | NAME == "Canada") %>%
  st_intersection(states)

super_users_state <- super_users_us %>%
  group_by(name) %>%
  count() %>%
  st_set_geometry(NULL)

super_users_map <- states %>%
  left_join(super_users_state) %>%
  mutate(log_sum = log10(n))

super_obs_us <- tm_shape(super_users_map) + tm_polygons(col = "log_sum", palette = "YlGnBu", title = "Log10(Observations)") +
  tm_layout(legend.text.size = 1, legend.title.size = 1.25)
tmap_save(super_obs_us, "figs/inaturalist/insect_super_obs_us_map.pdf")

# Tendency to submit repeat observations of the same species in a day, month, year
# Case study: Eastern Tiger Swallowtail
# All users, users who have submitted at least 20 observations

taxa_freq_db <- tbl(con, "inat") %>%
  select(scientific_name, iconic_taxon_name, latitude, longitude, user_login, id, observed_on, taxon_id, scientific_name) %>%
  filter(scientific_name == "Papilio glaucus") %>%
  mutate(year = substr(observed_on, 1, 4),
         month = substr(observed_on, 6, 7)) %>%
  group_by(year, month, observed_on, user_login) %>%
  summarize(n_obs = n_distinct(id))

taxa_freq_df <- taxa_freq_db %>%
  collect()
# 10046 observations

all_daily <- ggplot(taxa_freq_df, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Daily")

taxa_freq_month <- taxa_freq_df %>%
  group_by(year, month, user_login) %>%
  summarize(n_obs = sum(n_obs))

all_monthly <- ggplot(taxa_freq_month, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Monthly")

taxa_freq_year <- taxa_freq_df %>%
  group_by(year, user_login) %>%
  summarize(n_obs = sum(n_obs))

all_yearly <- ggplot(taxa_freq_year, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Yearly")

plot_grid(all_daily, all_monthly, all_yearly, nrow = 1)
ggsave("figs/inaturalist/rep_swallowtail_obs_all_users.pdf", height = 3, width = 9, units = "in")

taxa_freq_users <- taxa_freq_df %>%
  group_by(user_login) %>%
  filter(sum(n_obs) > 20)
# 1058 observations

freq_daily <- ggplot(taxa_freq_users, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Daily")

users_freq_month <- taxa_freq_users %>%
  group_by(year, month, user_login) %>%
  summarize(n_obs = sum(n_obs))

freq_monthly <- ggplot(users_freq_month, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Monthly")

users_freq_year <- taxa_freq_users %>%
  group_by(year, user_login) %>%
  summarize(n_obs = sum(n_obs))

freq_yearly <- ggplot(users_freq_year, aes(n_obs)) + stat_ecdf(geom= "step", cex = 1) +
  labs(x = "P. glaucus observations", y = "ECDF", title = "Yearly")

plot_grid(freq_daily, freq_monthly, freq_yearly, nrow = 1)
ggsave("figs/inaturalist/rep_swallowtail_obs_freq_users.pdf", height = 3, width = 9, units = "in")
  