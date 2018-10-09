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
                                          main = siteLabel, cex.main = .9)
  
  legend("topright", legend = paste(round(siteList$Latitude[siteList$Name == site], 1), "°N", sep = ""), 
         bty = 'n')
  legend("topleft", legend = paste(siteList$nRecs[siteList$Name == site], "surveys"), 
         bty = 'n', text.col = 'blue', cex = .9)
  axis(1, at = jds[minPos:maxPos], labels = F, tck = -.03)
  axis(1, at = jds[minPos:maxPos] + 14, labels = F, tck = -.02)
  
  monthLabs = minPos:(maxPos-1)
  rect(jds[monthLabs[monthLabs%%2 == 0]], rep(-10, length(monthLabs[monthLabs%%2 == 0])), 
       jds[monthLabs[monthLabs%%2 == 0] + 1]-1, rep(110, length(monthLabs[monthLabs%%2 == 0])), 
       col = rgb(.1, .1, .1, .1), border = NA)
  mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = .7, line = .25)
}
mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 1, cex = 1.5)
dev.off()


# Top 12 sites (but without ECU)
minNumRecords = 267 
minNumDates = 4

siteList = fullDataset %>%
  filter(Year == 2018) %>%
  group_by(Name, Region, Latitude) %>%
  summarize(nRecs = n_distinct(ID),
            nDates = n_distinct(LocalDate)) %>%
  arrange(desc(Latitude)) %>%
  filter(nRecs >= minNumRecords, nDates >= minNumDates, !Name %in% c("Example Site", "East Carolina University", "NC State University"))

pdf('figs/caterpillarPhenologySelectSites2018.pdf', height = 8.5, width = 11)
par(mfrow = c(3, 4), mar = c(3, 2, 3, 2), oma = c(5, 5, 0, 0))

for (site in siteList$Name) {
  sitedata = fullDataset %>%
    filter(Name == site, Year == 2018)
  
  if (nchar(site) > 26) {
    siteLabel = paste(substr(site, 1, 24), "\n", substr(site, 25, nchar(site)), 
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
                                          plot = TRUE, plotVar = 'fracSurveys', xlab = '',
                                          ylab = '', col = 'red', lwd = 3, 
                                          xaxt = 'n', cex.lab = 1.5, cex.axis = 1.5,
                                          xlim = c(jds[minPos], jds[maxPos]),
                                          ylim = c(0, max(1, 1.3*max(caterpillarPhenology$fracSurveys))), 
                                          main = siteLabel, cex.main = 1.5)
  
  legend("topright", legend = paste(round(siteList$Latitude[siteList$Name == site], 1), "°N", sep = ""), 
         bty = 'n', cex = 1.5)
  legend("topleft", legend = paste(siteList$nRecs[siteList$Name == site], "surveys"), 
         bty = 'n', text.col = 'blue', cex = 1.5)
  axis(1, at = jds[minPos:maxPos], labels = F, tck = -.03)
  axis(1, at = jds[minPos:maxPos] + 14, labels = F, tck = -.02)
  
  monthLabs = minPos:(maxPos-1)
  rect(jds[monthLabs[monthLabs%%2 == 0]], rep(-10, length(monthLabs[monthLabs%%2 == 0])), 
       jds[monthLabs[monthLabs%%2 == 0] + 1]-1, rep(110, length(monthLabs[monthLabs%%2 == 0])), 
       col = rgb(.1, .1, .1, .1), border = NA)
  mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = .9, line = .5)
}
mtext("Date", 1, outer = TRUE, line = 1.5, cex = 1.75)
mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 1.75, cex = 1.5)
dev.off()


# Top 12 sites without ECU, weighted mean fraction of surveys with caterpillars
## GD

library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

# Create nested data frame containing top 12 sites, get mean density by day of caterpillars for each
# Calculate mean peak in caterpillar abundance based on fraction of surveys
t12_nested <- fullDataset %>%
  filter(Name %in% siteList$Name) %>%
  group_by(Name) %>%
  nest() %>%
  mutate(meanDens = purrr::map(data, ~{meanDensityByDay(., ordersToInclude = 'caterpillar', 
                                     plot = FALSE, plotVar = 'fracSurveys')})) %>%
  mutate(weightedPeak = map_dbl(meanDens, ~{
    df <- .
    sum(df$fracSurveys*df$julianday)/sum(df$fracSurveys) # weighted mean of survey day by fraction of surveys with caterpillars
  })) %>%
  left_join(siteList) %>%
  left_join(unique(fullDataset[, c("Name", "Longitude")]))

## Plots 
theme_set(theme_bw())

# julian day labels
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

year <- data.frame(jds = jds, month = dates)
summer <- filter(year, jds > 100 & jds < 220)

# Plot of weighted mean peak in fraction of surveys with caterpillars by latitude
pdf('figs/weightedCaterpillarPeakScatter2018.pdf', height = 8.5, width = 11)
ggplot(t12_nested, aes(x = Latitude, y = weightedPeak)) + geom_point(aes(size = nRecs), col = "dodgerblue3") +
  labs(y = "Mean peak in caterpillars", size = "N. Surveys") + xlim(32, 49) + 
  scale_y_continuous(breaks = summer$jds, labels = summer$month)
# add site names: geom_text(aes(x = Latitude + 1, y = weightedPeak - 2, label = Name))
dev.off()

# Map of same info
us_states <- map_data("state")
eastern_states <- us_states %>%
  filter(region %in% c("michigan", "indiana", "kentucky", "tennessee", "north carolina", "virginia",
                       "west virginia", "ohio", "maryland", "delaware","new jersey", "pennsylvania",
                       "new york", "rhode island", "connecticut", "massachusetts", "new hampshire",
                       "vermont", "maine", "south carolina"))

pdf('figs/weightedCaterpillarPeakMap2018.pdf', height = 8.5, width = 11)
ggplot() + geom_polygon(data = eastern_states, aes(x = long, y = lat, group = group), fill = "gray88", color = "white") + 
  coord_map() +
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  geom_point(data = t12_nested, 
             aes(x = Longitude, y = Latitude, size = nRecs, color = weightedPeak)) +
  labs(x = NULL, y = NULL, size = "Number of surveys", color = "Mean peak in caterpillars") +
  scale_color_viridis(option = "C", breaks = summer$jds, labels = summer$month)
dev.off()
