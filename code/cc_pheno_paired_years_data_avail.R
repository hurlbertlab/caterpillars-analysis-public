## Survey effort for measuring year to year shifts in phenology in Caterpillars Count data

### Libraries
library(tidyverse)
library(cowplot)

theme_set(theme_classic(base_size = 15))

### Read in CC data and functions
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

### Site effort summary

site_effort <- data.frame(year = c(2015:2019)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDataset, year = y)
  })) %>%
  unnest(cols = c(site_effort))

## For nGoodWeeks 3-10, how many site/2year points in analysis?

years_good <- tibble(minWeeksGoodor50Surveys = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeksGoodor50Surveys, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeksGoodor50Surveys >= nweeks)
  }),
  n_siteyears = purrr::map_dbl(sites, ~nrow(.)),
  n_regions = purrr::map_dbl(sites, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(sites, ~length(unique(.$cell))))

goodweeks <- ggplot(years_good, aes(x = minWeeksGoodor50Surveys)) + 
  geom_line(aes(y = n_siteyears, col = "Site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = " ", x = "Minimum good weeks or weeks with 50 surveys") +
  theme(legend.position = c(0.8, 0.9))

## For nWeeks 3-10

yearpairs_all <- tibble(minWeeks = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeks, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeks >= nweeks)
  }),
  n_siteyears = purrr::map_dbl(sites, ~nrow(.)),
  n_regions = purrr::map_dbl(sites, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(sites, ~length(unique(.$cell))))

allweeks <- ggplot(yearpairs_all, aes(x = minWeeks)) + 
  geom_line(aes(y = n_siteyears, col = "Site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = "Data points", x = "Minimum weeks") +
  theme(legend.position = c(0.8, 0.9))

plot_grid(allweeks, goodweeks, ncol = 2)
ggsave("figs/caterpillars-count/pheno_data_sites_per_year.pdf", units = "in", height = 5, width = 10)

## If at least 8 good weeks

focal_sites <- years_good %>%
  filter(minWeeksGoodor50Surveys == 6) %>%
  unnest(cols = c("sites"))

focal_years_plot <-  focal_sites %>% 
  pivot_longer(firstGoodDate:lastGoodDate, names_to = "time", values_to = "dates")

# For sites with at least 8 good weeks, what is difference between mean jday of all surveys?

surveyThreshold = 0.8            # proprortion of surveys conducted to be considered a good sampling day
minJulianWeek = 102              # beginning of seasonal window for tabulating # of good weeks
maxJulianWeek = 214

## Keep just good weeks
## This is broken - means are the same each year
meanJday <- fullDataset %>%
  right_join(focal_sites) %>%
  group_by(Name, Region, year, julianweek) %>%
  summarize(nSurveysPerWeek = n_distinct(ID)) %>%
  group_by(Name, Region, year) %>%
  mutate(medianSurveysPerWeek = median(nSurveysPerWeek, na.rm = T)) %>%
  filter(julianweek >= minJulianWeek & julianweek <= maxJulianWeek & nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek) %>%
  group_by(year, Name) %>%
  summarize(meanJday = mean(julianweek))

# plot panel for each site with at least 8 good weeks
# year vs. jday, lines from firstGoodDate to lastGoodDate

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(focal_years_plot) + geom_path(aes(x = dates, y = year, group = year), size = 2) + 
  scale_x_continuous(breaks = jds, labels = dates) + facet_wrap(~Name) +
  labs(x = " ", y = " ")
ggsave("figs/caterpillars-count/pheno_siteyears_overlap.pdf", units = "in", height = 10, width = 15)




