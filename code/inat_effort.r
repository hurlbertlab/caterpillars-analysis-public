# Script for counting unique user-days or user-hours from iNat data

library(tidyverse)
library(lubridate)

inatcat = read.csv('data/inat_caterpillars_easternNA.csv', header = T, quote = '\"', fill = TRUE, stringsAsFactors = FALSE)

dat = inatcat %>%
  filter(user_login != 'caterpillarscount') %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  count(Date, user_login) %>%
  count(n)

#### Observer-days and observer-hours for Eastern North America - Hymenoptera, Hempitera, Araneae, Coleoptera, Orthoptera iNat obs

# Read in data
inat_1 <- read.csv("C:/Users/gdicecco/Desktop/data/iNaturalist_arthropods/observations-56952.csv")
inat_2 <- read.csv("C:/Users/gdicecco/Desktop/data/iNaturalist_arthropods/observations-56953.csv")
inat_3 <- read.csv("C:/Users/gdicecco/Desktop/data/iNaturalist_arthropods/observations-56954.csv")
inat_4 <- read.csv("C:/Users/gdicecco/Desktop/data/iNaturalist_arthropods/observations-56955.csv")
inat_5 <- read.csv("C:/Users/gdicecco/Desktop/data/iNaturalist_arthropods/observations-56956.csv")

## Observer-days
obs_effort_arth <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  count(Date, user_login) %>%
  count(n) %>%
  rename("obs_days" = n,
         "count" = nn)

write.csv(obs_effort_arth,"data/inaturalist_observer_days.csv", row.names = F)

ggplot(obs_effort_arth, aes(x = obs_days, y = count)) + geom_col(width = 0.1) + theme_classic() + scale_x_log10() +
  theme(axis.title.x = element_text(size = 14), axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14))
ggsave("figs/inaturalist/observer-days.pdf")

# Observer-days by week for 2018

obs_effort_2018 <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date),
         jday = yday(Date),
         jd_wk = 7*floor(jday/7)) %>%
  filter(year == 2018) %>%
  group_by(jd_wk) %>%
  summarize(obs_days = n_distinct(Date, user_login)) 

write.csv(obs_effort_2018, "data/inaturalist_observer_days_2018.csv", row.names = F)

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(obs_effort_2018, aes(x = jd_wk, y = obs_days)) + geom_col(col = "white") + theme_classic() +
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14)) +
  scale_x_continuous(breaks = jds, labels = dates)
ggsave("figs/inaturalist/observer-days-2018.pdf")

## Add 2015, 2016, 2017
cnc <- data.frame(year = c(2015:2018), cnc_date = c(NA, 103, 108, 119))

obs_effort_year <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date),
         jday = yday(Date),
         jd_wk = 7*floor(jday/7)) %>%
  filter(year >= 2015, year < 2019) %>%
  group_by(year, jd_wk) %>%
  summarize(obs_days = n_distinct(Date, user_login)) %>%
  left_join(cnc, by = "year")

ggplot(obs_effort_year, aes(x = jd_wk, y = obs_days)) + geom_col(col = "white") + theme_classic() +
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), strip.text = element_text(size = 14)) +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(y = "Observer-days") +
  geom_vline(aes(xintercept = cnc_date), lty = 2, color = "red") +
  geom_label(aes(x = cnc_date, y = 6000, label = "City Nature Challenge")) +
  facet_wrap(~year)
ggsave("figs/inaturalist/observer-days-by-year.pdf", units = "in", height = 6, width = 10)

## Observer-days by week for 2017, 2018, by lat-lon bin
obs_effort_geog <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date),
         jday = yday(Date),
         jd_wk = 7*floor(jday/7),
         lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2) %>%
  filter(year >= 2017, year < 2019) %>%
  group_by(lat_bin, lon_bin, year, jd_wk) %>%
  summarize(obs_days = n_distinct(Date, user_login)) 
write.csv(obs_effort_geog, "data/inaturalist_observer_days_by_latlon.csv", row.names = F)

## Observer-hours
obs_hours <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  mutate(time = word(time_observed_at, 2),
         hour = word(time, 1, sep = ":")) %>%
  distinct(Date, hour, user_login) %>%
  count(Date, user_login) %>%
  count(n) %>%
  rename("obs_hours" = n,
         "count" = nn)

write.csv(obs_hours, "data/inaturalist_observer_hours.csv", row.names = F)

ggplot(obs_hours, aes(x = obs_hours, y = count)) + geom_col(width = 0.1) + theme_classic() + scale_x_log10() +
  theme(axis.title.x = element_text(size = 14), axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14))
ggsave("figs/inaturalist/observer-hours.pdf")

### Moving-window parameter estimates of observations vs. observer-days

raw_obs_effort <- bind_rows(inat_1, inat_2, inat_3, inat_4, inat_5) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date),
         jday = yday(Date),
         jd_wk = 7*floor(jday/7)) %>%
  filter(year == 2018 | year == 2017) %>%
  group_by(year, jd_wk) %>%
  summarize(obs_days = n_distinct(Date, user_login),
            observations = n()) 

lm_rolling <- rollapply(raw_obs_effort, 
                        width = 8,
                        FUN = function(z) summary(lm(observations ~ obs_days, data = as.data.frame(z)))$coefficients[-1, ],
                        by.column = F, align = "right")

rolling_windows <- rollapply(raw_obs_effort, 
                             width = 8,
                             FUN = function(z) c(min = as.data.frame(z)$jd_wk[1], max = as.data.frame(z)$jd_wk[8]),
                             by.column = F, align = "right")

# City nature challenge - jday 119 in 2018, 112 in 2017

rolling_df <- data.frame(lm_rolling) %>%
  mutate(ci = 1.96*Std..Error,
         window = as.numeric(row.names(.)))

theme_set(theme_classic())
ggplot(rolling_df, aes(x = window, y = Estimate)) +
  labs(x = "Time Window") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_vline(xintercept = 13, lty = 2, col = "red") +
  geom_vline(xintercept = 25, lty = 2) +
  geom_vline(xintercept = 50, lty = 2) +
  geom_vline(xintercept = 67, lty = 2, col = "red") +
  geom_vline(xintercept = 75, lty = 2) +
  geom_vline(xintercept = 99, lty = 2) +
  annotate(geom = "label", x = 99, y = -1, label = "Dec 2018") +
  annotate(geom = "label", x = 75, y = -1, label = "June 2018") +
  annotate(geom = "label", x = 50, y = -1, label = "Dec 2017 - Jan 2018") +
  annotate(geom = "label", x = 25, y = -1, label = "June 2017") +
  annotate(geom = "label", x = 13, y = 4.3, label = "City Nature Challenge") +
  annotate(geom = "label", x = 67, y = 4.3, label = "City Nature Challenge") +
  annotate(geom = "label", x = 1, y = -1, label = "Jan 2017") +
  geom_errorbar(aes(ymin = Estimate - ci, ymax = Estimate + ci), width = 0, col = "gray") +
  geom_point()
ggsave("figs/inaturalist/moving_window_obs_effort_slopes.pdf")

### Moving window analysis of caterpillar obs vs. observer days

cat_obs <- inatcat %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d"),
         year = year(Date),
         jday = yday(Date),
         jd_wk = 7*floor(jday/7)) %>%
  filter(year == 2018 | year == 2017) %>%
  group_by(year, jd_wk) %>%
  summarize(observations = n()) 

cat_obs_effort <- raw_obs_effort %>%
  dplyr::select(-observations) %>%
  left_join(cat_obs, by = c("year", "jd_wk"))

lm_rolling_cat <- rollapply(cat_obs_effort, 
                        width = 8,
                        FUN = function(z) summary(lm(observations ~ obs_days, data = as.data.frame(z)))$coefficients[-1, ],
                        by.column = F, align = "right")

# City nature challenge - jday 119 in 2018, 112 in 2017

rolling_cat_df <- data.frame(lm_rolling_cat) %>%
  mutate(ci = 1.96*Std..Error,
         window = as.numeric(row.names(.)))

theme_set(theme_classic())
ggplot(rolling_cat_df, aes(x = window, y = Estimate)) +
  labs(x = "Time Window") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_vline(xintercept = 13, lty = 2, col = "red") +
  geom_vline(xintercept = 25, lty = 2) +
  geom_vline(xintercept = 50, lty = 2) +
  geom_vline(xintercept = 67, lty = 2, col = "red") +
  geom_vline(xintercept = 75, lty = 2) +
  geom_vline(xintercept = 99, lty = 2) +
  annotate(geom = "label", x = 99, y = -1, label = "Dec 2018") +
  annotate(geom = "label", x = 75, y = -1, label = "June 2018") +
  annotate(geom = "label", x = 50, y = -1, label = "Dec 2017 - Jan 2018") +
  annotate(geom = "label", x = 25, y = -1, label = "June 2017") +
  annotate(geom = "label", x = 13, y = 2, label = "City Nature Challenge") +
  annotate(geom = "label", x = 67, y = 2, label = "City Nature Challenge") +
  annotate(geom = "label", x = 1, y = -1, label = "Jan 2017") +
  geom_errorbar(aes(ymin = Estimate - ci, ymax = Estimate + ci), width = 0, col = "gray") +
  geom_point()
ggsave("figs/inaturalist/moving_window_obs_effort_slopes_caterpillars.pdf")
