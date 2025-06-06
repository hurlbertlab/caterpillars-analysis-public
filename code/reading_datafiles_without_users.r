# Reading in Caterpillars Count! database files
library(dplyr)
library(lubridate)
library(sf)
library(rvest)
library(xml2)
library(stringr)

source('code/analysis_functions.r')

# Slope and intercept parameters for power function length-weight regressions for different arthropod groups
massregs = read.csv('data/arthropod_length_weight_regressions.csv', header = TRUE, stringsAsFactors = FALSE)

# Read in raw data files

## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/")) %>%
  distinct()


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

# Read in official plant list
plantSpecieswebpage <- read_html("https://github.com/hurlbertlab/caterpillars-count-data/tree/master/plantSpecies")
plant_repo_links <- html_attr(html_nodes(plantSpecieswebpage, "a"), "href")
plant_data_links <- tibble(link = plant_repo_links[grepl("officialPlant", plant_repo_links)]) %>%
  mutate(file_name = word(link, 7, 7, sep = "/")) %>%
  distinct()

mostRecentOfficialPlantList = plant_data_links$file_name[nrow(plant_data_links)]
officialPlantList = read.csv(paste0(github_raw, 'plantSpecies/', mostRecentOfficialPlantList), 
                             header = T, quote = '\"', fill = TRUE)


# Join it all together
fullDataset = surveys %>%
  dplyr::select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, julianweek, Year, ObservationMethod, Notes, WetLeaves, PlantSpecies, NumberOfLeaves,
         AverageLeafLength, HerbivoryScore) %>%
  left_join(arths[, names(arths) != "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
  left_join(plants[, c('ID', 'SiteFK', 'Circle', 'Orientation', 'Code', 'IsConifer', 'Species')], by = c('PlantFK' = 'ID')) %>%
  left_join(officialPlantList[, c('userPlantName', 'sciName', 'genus', 'Family', 'rank')], by = c('Species' = 'userPlantName')) %>%
  left_join(sites[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region')], by = c('SiteFK' = 'ID')) %>% 
  mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
  mutate_cond(is.na(Biomass_mg), Biomass_mg = 0, Group) %>%
  rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y, plantRank = rank, plantGenus = genus) %>%
  filter(Name != "Example Site")


write.csv(fullDataset, paste('data/fullDataset_', Sys.Date(), '.csv', sep = ''), row.names = F)
