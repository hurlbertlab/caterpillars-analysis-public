# This file creates a vertically stacked set of caterpillar phenology curves
# based on Caterpillars Count! data from 2018 for an AGU poster

library(dplyr)
library(lubridate)
library(raster)

source('code/analysis_functions.r')


# Read in sites
phenosites = read.table('data/revi/sitelist2018_revi.txt', header = T, sep = '\t', quote = '\"') %>%
  mutate(preArrival = yday(as.Date(LatestWeekWithFreq0, format = "%m/%d/%Y")) + 3, # +3 to shift from beg to middle of week
         peakArrival = yday(as.Date(WeekOfPeakFreq, format = "%m/%d/%Y")) + 3,
         arrival = round((preArrival + peakArrival)/2),
         hatching = arrival + 35,
         fledging = hatching + 11) %>%
  dplyr::select(Name:nDates, preArrival, peakArrival, arrival, hatching, fledging) %>%
  filter(!Name %in% c('Wellesley College', "The Children's Museum", "Curritucks Banks Reserve", "Ijams Nature Center", 
                      'NC State University', 'Hemlock Bluffs', 'East Carolina University'))

sitePoints = phenosites[, c('Longitude', 'Latitude')]

# Read in GDD
# For some reason, degrees longitude are in degrees E, so need to add 360
sitePoints2 = sitePoints %>%
  mutate(Longitude = Longitude + 360)
gdd = raster('data/maps/2018GDD.tif')
phenosites$gdd600jd = extract(gdd, sitePoints2)

# Read in NPN spring indices
npn = raster('data/maps/data.tif')
phenosites$leafIndex = extract(npn, sitePoints)

# Fill in spring index for RVCC based on nearest US coordinates:
phenosites$leafIndex[phenosites$Name == 'RVCC'] = extract(npn, data.frame(long = -78.975473, lat = 43.23961))

# Working with CC data

# Read in data files
sites = read.csv(paste('data/', list.files('data')[grep('Site.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

surveys = read.csv(paste('data/', list.files('data')[grep('Survey.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

arths = read.csv(paste('data/', list.files('data')[grep('ArthropodSighting.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

plants = read.csv(paste('data/', list.files('data')[grep('Plant.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)

surveys$LocalDate = as.Date(surveys$LocalDate, format = "%Y-%m-%d")
surveys$Year = format(surveys$LocalDate, "%Y")
surveys$julianday = yday(surveys$LocalDate)


fullDataset = surveys %>%
  select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, Year, 
         ObservationMethod, Notes, WetLeaves, PlantSpecies, NumberOfLeaves,
         AverageLeafLength, HerbivoryScore) %>%
  left_join(arths[, !names(arths) %in% "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  left_join(sites[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region')], by = c('SiteFK' = 'ID')) %>% 
  mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
  rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y)


# Figure of caterpillar phenology at each site, stacked vertically from the highest
# to lowest latitudes, with indications of date of accumulation of 600 GDD,
# date of spring leaf out, and date of REVI ...

# x-axis range
jdMin = 91
jdMax = 228

pdf('figs/stacked_cat_gdd_greenup_revi_phenology_2018.pdf', height = 20, width = 6)
par(mfrow = c(nrow(phenosites), 1), mar = c(0.3, 1, 1.3, 1), oma = c(5, 5, 0, 0))

i = 1

for (site in phenosites$Name) {
  sitedata = fullDataset %>%
    filter(Name == site, Year == 2018)
  
  # x-axis labels
  jds = c(91, 121, 152, 182, 213)
  dates = c("Apr 1", "May 1", "Jun 1", "Jul 1", "Aug 1")
  
  # Caterpillar phenology
  #  - first find out what the range of values is before plotting
  caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                          plot = FALSE, plotVar = 'fracSurveys')
  
  
  caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                          plot = TRUE, plotVar = 'fracSurveys', xlab = 'Date',
                                          ylab = 'Percent of surveys', col = 'black', lwd = 3, 
                                          xaxt = 'n', cex.lab = 1.5, cex.axis = .9,
                                          xlim = c(jdMin, jdMax),
                                          ylim = c(0, max(1, 1.1*max(caterpillarPhenology$fracSurveys))), 
                                          main = '', 
                                          cex.main = .9)
  mtext(paste(site, ", ", round(phenosites$Latitude[phenosites$Name == site], 1), "°N;   ",
              phenosites$nRecs[phenosites$Name == site], "surveys", sep = ""), 3, line = 0, cex = .8)
  
  axis(1, at = jds, labels = F, tck = -.1)
  axis(1, at = jds + 14, labels = F, tck = -.05)
  
  # Shaded rectangles
  monthPos = 1:4
  
  rect(jds[monthPos[monthPos%%2 == 0]], rep(-10, length(monthPos[monthPos%%2 == 0])), 
       jds[monthPos[monthPos%%2 == 0] + 1]-1, rep(110, length(monthPos[monthPos%%2 == 0])), 
       col = rgb(.05, .05, .05, .03), border = NA)
  
  
  # Add reference lines
  abline(v = phenosites$gdd600jd[phenosites$Name == site], col = 'cornflowerblue', lwd = 3)
  abline(v = phenosites$leafIndex[phenosites$Name == site], col = 'lightgreen', lwd = 3)
  abline(v = phenosites$hatching[phenosites$Name == site], col = 'tomato', lwd = 3)
  
  if (i == 1) {
    text(jds, rep(0.5, length(jds)), dates, cex = .8)
    i = i + 1
  }
    
}
mtext(dates, 1, at = jds, cex = .7, line = .25)
mtext("Date", 1, outer = TRUE, line = 3, cex = 1.5)
mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 2.5, cex = 1.5)
dev.off()
