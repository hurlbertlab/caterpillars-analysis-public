## Survey effort for measuring year to year shifts in phenology in Caterpillars Count data

### Libraries
library(tidyverse)
library(cowplot)

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

yearpairs_good <- tibble(minGoodWeeks = c(3:10)) %>%
  mutate(sites = purrr::map(minGoodWeeks, ~{
    nweeks <- .
    site_effort %>%
      filter(nGoodWeeks >= nweeks)
  }),
  consec_years = purrr::map(sites, ~{
    df <- .
    df %>%
      group_by(Name) %>%
      arrange(year) %>%
      mutate(consec_year = lead(year) - year) %>%
      filter(consec_year == 1)
  }),
  n_yearpairs = purrr::map_dbl(consec_years, ~nrow(.)),
  n_regions = purrr::map_dbl(consec_years, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(consec_years, ~length(unique(.$cell))))

goodweeks <- ggplot(yearpairs_good, aes(x = minGoodWeeks)) + 
  geom_line(aes(y = n_yearpairs, col = "Paired site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = " ", x = "Minimum good weeks") +
  ylim(c(0, 40)) +
  theme(legend.position = c(0.8, 0.9))

## For nWeeks 3-10

yearpairs_all <- tibble(minWeeks = c(3:10)) %>%
  mutate(sites = purrr::map(minWeeks, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeks >= nweeks)
  }),
  consec_years = purrr::map(sites, ~{
    df <- .
    df %>%
      group_by(Name) %>%
      arrange(year) %>%
      mutate(consec_year = lead(year) - year) %>%
      filter(consec_year == 1)
  }),
  n_yearpairs = purrr::map_dbl(consec_years, ~nrow(.)),
  n_regions = purrr::map_dbl(consec_years, ~length(unique(.$Region))),
  n_cells = purrr::map_dbl(consec_years, ~length(unique(.$cell))))

allweeks <- ggplot(yearpairs_all, aes(x = minWeeks)) + 
  geom_line(aes(y = n_yearpairs, col = "Paired site-years"), cex = 1) +
  geom_line(aes(y = n_regions, col = "Regions"), cex = 1) +
  geom_line(aes(y = n_cells, col = "Hex cells"), cex = 1) +
  labs(col = "", y = "Data points", x = "Minimum weeks") +
  ylim(c(0, 40)) +
  theme(legend.position = c(0.8, 0.9))

plot_grid(allweeks, goodweeks, ncol = 2)
ggsave("figs/caterpillars-count/pheno_data_paired_years.pdf", units = "in", height = 5, width = 10)
