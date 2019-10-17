# Reading in Caterpillars Count! database files
library(dplyr)
library(lubridate)
library(raster)

# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# Slope and intercept parameters for power function length-weight regressions for different arthropod groups
massregs = read.csv('data/arthropod_length_weight_regressions.csv', header = TRUE, stringsAsFactors = FALSE)



# Read in data files
sites = read.csv(paste('data/', list.files('data')[grep('Site.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

surveys = read.csv(paste('data/', list.files('data')[grep('Survey.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

plants = read.csv(paste('data/', list.files('data')[grep('Plant.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

arths = read.csv(paste('data/', list.files('data')[grep('ArthropodSighting.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE) %>%
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

# Note there are still a few sites with no greenup data including 
#   RVCC, Beare Swamp in Rouge Park, and Wye Marsh Wildlife Centre

fullDataset = surveys %>%
  dplyr::select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, julianweek, Year, ObservationMethod, Notes, WetLeaves, PlantSpecies, NumberOfLeaves,
         AverageLeafLength, HerbivoryScore) %>%
  left_join(arths[, names(arths) != "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  left_join(sites[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region', 'medianGreenup')], by = c('SiteFK' = 'ID')) %>% 
  mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
  mutate_cond(is.na(Biomass_mg), Biomass_mg = 0, Group) %>%
  rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y)
