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

ggplot(obs_effort_2018, aes(x = jd_wk, y = obs_days)) + geom_col() + theme_classic() +
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14)) +
  scale_x_continuous(breaks = jds, labels = dates)
ggsave("figs/inaturalist/observer-days-2018.pdf")

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
