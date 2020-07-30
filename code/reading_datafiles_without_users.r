# Reading in Caterpillars Count! database files
library(dplyr)
library(lubridate)
library(rgdal)
library(raster)
library(dggridR)
library(sf)
library(rvest)
library(xml2)
library(stringr)

source('code/analysis_functions.r')

# Slope and intercept parameters for power function length-weight regressions for different arthropod groups
massregs = read.csv('data/arthropod_length_weight_regressions.csv', header = TRUE, stringsAsFactors = FALSE)

# Read in hex grid data
hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp")


# Read in data files

## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/"))


## Read data files from data repo links
github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

sites = read.csv(paste(github_raw, filter(data_links, grepl("Site.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)

surveys = read.csv(paste(github_raw, filter(data_links, grepl("Survey.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)

plants = read.csv(paste(github_raw, filter(data_links, grepl("Plant.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)

arths = read.csv(paste(github_raw, filter(data_links, grepl("ArthropodSighting.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE) %>%
  rename(Group = "UpdatedGroup", BeetleLarva = "UpdatedBeetleLarva", Sawfly = "UpdatedSawfly") %>%
  left_join(massregs, by = 'Group') %>%
  mutate(Biomass_mg = Quantity*a_constant*Length^b_exponent, 
         Photo = ifelse(PhotoURL == "", 0, 1)) %>%
  dplyr::select(ID:BeetleLarva, Biomass_mg, Photo)
           

surveys$LocalDate = as.Date(surveys$LocalDate, format = "%Y-%m-%d")
surveys$Year = as.numeric(format(surveys$LocalDate, "%Y"))
surveys$julianday = yday(surveys$LocalDate)
surveys$julianweek = 7*floor(surveys$julianday/7) + 4


# Median green up date for 2001-2017 based on MODIS MCD12Q2 v006
# downloaded from USANPN.org gridded products
greenup = raster("data/env/inca_midgup_median_nad83_02deg.tif")
sites$medianGreenup = round(extract(greenup, sites[, c('Longitude', 'Latitude')]))

# Manually get median green up for Currituck Banks and Sault College which fall just outside of raster cells
sites$medianGreenup[sites$Name == "Currituck Banks Reserve"] = 
  round(mean(unlist(extract(greenup, data.frame(longitude = sites$Longitude[sites$Name == "Currituck Banks Reserve"], 
                                                latitude = sites$Latitude[sites$Name == "Currituck Banks Reserve"]),
                            buffer = 3000)), na.rm = TRUE))

sites$medianGreenup[sites$Name == "Sault College"] = 
  round(mean(unlist(extract(greenup, data.frame(longitude = sites$Longitude[sites$Name == "Sault College"], 
                                                latitude = sites$Latitude[sites$Name == "Sault College"]),
                            buffer = 7000)), na.rm = TRUE))

# One of the Acadia NP sites falls just off the raster coverage, assign it same value as its neighbor:
sites$medianGreenup[sites$Name == "Acadia NP - Alder"] = sites$medianGreenup[sites$Name == "Acadia NP - Sundew"]

# Add county info based on lat-longs
sites$county = latlong2county(sites[, c('Longitude', 'Latitude')])

# Canadian sites and a few other exceptions need to be added manually
sites$county = case_when(sites$Name == 'Sault College' ~ 'ontario,algoma',
                         sites$Name %in% c('RVCC', 'Beare Swamp in Rouge Park') ~ 'ontario,toronto',
                         sites$Name == 'Linda Loring Nature Foundation' ~ 'massachusetts,nantucket',
                         sites$Name %in% c('Acadia NP - Alder', 'Acadia NP - Sundew') ~ 'maine,hancock',
                         sites$Name == 'Wye Marsh Wildlife Centre' ~ 'ontario,simcoe',
                         sites$Name == 'Riverbend Park' ~ 'virginia,fairfax',
                         !is.na(sites$county) ~ sites$county)

# Join eBird county codes. As new CC! sites arise in new counties, they will need to be added manually
# to this countyCodes.txt file (could request full table from eBird...)
countyCodes = read.table('data/countyCodes.txt', sep = '\t', header = T, stringsAsFactors = F)
sites2 = left_join(sites, countyCodes, by = 'county')

# Number of weeks per site per year per hex cell
hexcells <- sites %>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersection(hex)   

sites3 = left_join(sites2, hexcells[, c('Name', 'cell')], by = 'Name')



# Note there are still a few sites with no greenup data including 
#   RVCC, Beare Swamp in Rouge Park, and Wye Marsh Wildlife Centre

fullDataset = surveys %>%
  dplyr::select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, julianweek, Year, ObservationMethod, Notes, WetLeaves, PlantSpecies, NumberOfLeaves,
         AverageLeafLength, HerbivoryScore) %>%
  left_join(arths[, names(arths) != "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  left_join(sites3[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region', 'medianGreenup', 'ebirdCounty', 'cell')], by = c('SiteFK' = 'ID')) %>% 
  mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
  mutate_cond(is.na(Biomass_mg), Biomass_mg = 0, Group) %>%
  rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y) %>%
  filter(Name != "Example Site")
