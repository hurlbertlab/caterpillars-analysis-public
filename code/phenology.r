# Script for displaying caterpillar phenology across sites

library(dplyr)
library(lubridate)

source('code/analysis_functions.r')


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

# Criteria for inclusion (records refers to survey events)
minNumRecords = 40 
minNumDates = 4

siteList = fullDataset %>%
  filter(Year == 2018) %>%
  group_by(Name, Region, Latitude) %>%
  summarize(nRecs = n_distinct(ID),
            nDates = n_distinct(LocalDate)) %>%
  arrange(desc(Latitude)) %>%
  filter(nRecs >= minNumRecords, nDates >= minNumDates, Name != "Example Site")

pdf('figs/caterpillarPhenologyAllSites2018.pdf', height = 8.5, width = 11)
par(mfrow = c(4, 6), mar = c(2, 2, 2, 1), oma = c(5, 5, 0, 0))

for (site in siteList$Name) {
  sitedata = fullDataset %>%
    filter(Name == site, Year == 2018)
  
  if (nchar(site) > 26) {
    siteLabel = paste(substr(site, 1, 21), "\n", substr(site, 22, nchar(site)), 
                      ", ", siteList$Region[siteList$Name == site], sep = "")
  } else {
    siteLabel = paste(site, ", ", siteList$Region[siteList$Name == site], sep = "")
  }
  
  
  # x-axis labels
  jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # make sure xlim endpoints coincide with month labels
  if(length(unique(sitedata$julianday)) == 1) {
    minPos = which(jds == max(jds[jds <= min(sitedata$julianday)]))
    maxPos = which(jds == min(jds[jds >= max(sitedata$julianday)]))
  } else {
    minPos = max(which(jds == min(jds[jds >= min(sitedata$julianday)])) - 1, 1)    
    maxPos = min(which(jds == max(jds[jds <= max(sitedata$julianday)])) + 1, 12)
  }
  
  # Caterpillar phenology
  caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                          plot = FALSE, plotVar = 'fracSurveys')
  
  
  caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                          plot = TRUE, plotVar = 'fracSurveys', xlab = 'Date',
                                          ylab = 'Percent of surveys', col = 'red', lwd = 3, 
                                          xaxt = 'n', cex.lab = 1.5, cex.axis = 1.3,
                                          xlim = c(jds[minPos], jds[maxPos]),
                                          ylim = c(0, max(1, 1.3*max(caterpillarPhenology$fracSurveys))), 
                                          main = siteLabel, cex.main = 1)
  
  legend("topright", legend = round(siteList$Latitude[siteList$Name == site], 1), bty = 'n')
  legend("topleft", legend = siteList$nRecs[siteList$Name == site], bty = 'n', text.col = 'blue')
  axis(1, at = jds[minPos:maxPos], labels = F, tck = -.03)
  axis(1, at = jds[minPos:maxPos] + 14, labels = F, tck = -.02)
  
  monthLabs = minPos:(maxPos-1)
  rect(jds[monthLabs[monthLabs%%2 == 0]], rep(-10, length(monthLabs[monthLabs%%2 == 0])), 
       jds[monthLabs[monthLabs%%2 == 0] + 1]-1, rep(110, length(monthLabs[monthLabs%%2 == 0])), 
       col = rgb(.2, .2, .2, .2), border = NA)
  mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = .7, line = .25)
}
mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
mtext("Percent of surveys", 2, outer = TRUE, line = 1, cex = 1.5)
dev.off()
