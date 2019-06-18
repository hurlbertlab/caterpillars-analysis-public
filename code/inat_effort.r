# Script for counting unique user-days or user-hours from iNat data

library(dplyr)

inatcat = read.csv('data/inat_caterpillars_easternNA.csv', header = T, quote = '\"', fill = TRUE, stringsAsFactors = FALSE)

dat = inatcat %>%
  filter(user_login != 'caterpillarscount') %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  count(Date, user_login) %>%
  count(n)
