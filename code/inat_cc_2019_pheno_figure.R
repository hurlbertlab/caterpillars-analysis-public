## Comparing CC and iNat phenology peaks and centroids in 2019 with temperature means

### Libraries
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(maps)
library(rgdal)
library(sf)
library(tmap)
library(forcats)
library(dggridR)
library(grid)
library(cowplot)

theme_set(theme_classic(base_size = 15))

### Read in CC data and functions
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

### Read in iNat data and mapping files

inat = read.csv('data/inat_caterpillars_eastern_NA_5-20-2020.csv', header = TRUE, stringsAsFactors = F)

NAmap = read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/inat_caterpillar_species_traits.txt", header = T, sep = "\t")

hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")

### Site effort summary

site_effort <- data.frame(year = c(2015:2019)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDataset, year = y)
  })) %>%
  unnest(cols = c(site_effort))

# CC: use good or 50 surveys weeks from cc_inat_pheno_anomalies filters w/o 2 year minimum

cc_sites <- tibble(minWeeksGoodor50Surveys = c(6)) %>%
  mutate(sites = purrr::map(minWeeksGoodor50Surveys, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeksGoodor50Surveys >= nweeks, modalSurveyBranches >= 20, year == 2019)
  }))

hex_cells_2019 <- unique(cc_sites$sites[[1]]$cell)

## Find 6 week common time windows

## calculate peak date and cenroid date

# iNaturalist
# Weekly insect observations by hex cell for the inclusive weeks per hex cell from site_overlap

# data/inat_2019_weekly_insecta_hex.csv

# Create effort corrected caterpillar pheno for inat 

## Peak and centroid dates 

## Figure 4:
## Panel 1
# Hex cells with iNat peak date

## Panel 2
# Hex cells with iNat centroid date

## Panel 3
# Hex cells with CC peak date

## panel 4
# Hex cells with CC centroid date

## Panel 5
# Scatterplot showing pheno metrics (col = dataset, symbol = metric) vs latitude (center of cell)

## Panel 6
# Scatterplot showing pheno metrics (col = dataset, symbol = metric) vs mean temperature