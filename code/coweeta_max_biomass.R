## Coweeta phenometrics
## Sites BB and BS

library(tidyverse)
library(purrr)
theme_set(theme_classic(base_size = 15))

source("code/analysis_functions.r")
source("code/reading_datafiles_without_users.r")

coweeta <- fullDataset %>%
  filter(Name == "Coweeta - BB" | Name == "Coweeta - BS") %>%
  group_by(Year, Name) %>%
  nest() %>%
  mutate(mean_dens_df = purrr::map(data, ~{
    meanDensityByWeek(., ordersToInclude = "caterpillar")
  })) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(mean_dens_df))

jds = c(121, 152, 182, 213)
dates = c("May", "Jun", "Jul", "Aug")

ggplot(coweeta, aes(x = julianweek, y = meanBiomass, col = Name)) + 
  geom_vline(xintercept = jds) +
  geom_line(cex = 1) + 
  geom_point() +
  scale_x_continuous(breaks = jds, labels = dates) +
  facet_wrap(~Year)

coweeta_max <- coweeta %>%
  filter(Name == "Coweeta - BS") %>%
  filter(Year <= 2016 & Year >= 2007) %>%
  group_by(Year) %>%
  summarize(max180 = julianweek[meanBiomass == max(meanBiomass[julianweek < 180])],
            max170 = julianweek[meanBiomass == max(meanBiomass[julianweek < 170])])
