# Script for displaying caterpillar phenology across sites

library(dplyr)
library(lubridate)
library(sp)
library(maps)
library(maptools)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')


#########################
# Map and rainbow phenocurves

# Base map
NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')
pdf('figs/basemap_easternNA.pdf', height = 12, width = 16)
plot(NAmap, xlim = c(-100, -64), ylim = c(25, 50), border = 'gray80', col = 'gray90')
points(siteList$Longitude, siteList$Latitude, pch = 16, cex = 2)
dev.off()

# Rainbow plots for sites

for (s in siteList$Name) {
  sitedata = fullDataset %>%
    filter(Name == s, Year == 2018)
  
  byday = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                           plot = FALSE, plotVar = 'fracSurveys')
  
  interp = interpolatePhenoByDay(byday)
  
  png(paste('figs/rainbowPheno_', s, '_2018.png', sep = ''), height = 600, width = 800, bg = NA)
  rainbowPhenoPlot(interp, lwd = 10)
  mtext(s, 3, cex = 3)
  dev.off()
}


###############################
# Phenology summaries
fullPhenoSummary = phenoSummary(fullDataset, ordersToInclude = 'caterpillar', postGreenupBeg = 30, postGreenupEnd = 75, minNumWeeks = 5)



# Top 12 sites without ECU, weighted mean fraction of surveys with caterpillars
## GD

library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

# Create nested data frame containing top 12 sites, get mean density by day of caterpillars for each
# Calculate mean peak and absolute peak in % surveys containing caterpillars
t12_nested <- fullDataset %>%
  filter(Name %in% siteList$Name, Year == 2018) %>%
  group_by(Name) %>%
  nest() %>%
  mutate(meanDens = purrr::map(data, ~{meanDensityByDay(., ordersToInclude = 'caterpillar', 
                                     plot = FALSE, plotVar = 'fracSurveys')})) %>%
  mutate(weightedPeak = map_dbl(meanDens, ~{
    df <- .
    sum(df$fracSurveys*df$julianday)/sum(df$fracSurveys) # weighted mean of survey day by fraction of surveys with caterpillars
  })) %>%
  mutate(minJD = map_dbl(meanDens, ~{
    df <- .
    min(df$julianday) # First survey date
  })) %>%
  mutate(maxJD = map_dbl(meanDens, ~{
    df <- .
    max(df$julianday) # Last survey date
  })) %>%
  mutate(weightedPeakJunJul = map_dbl(meanDens, ~{
    df <- .
    commonTime <- filter(df, julianday >= 152 & julianday <= 212) # JD time period all surveys overlap - June and July
    sum(commonTime$fracSurveys*commonTime$julianday)/sum(commonTime$fracSurveys) # weighted mean of survey day by fraction of surveys with caterpillars
  })) %>%
  mutate(peak = map_dbl(meanDens, ~{
    df <- .
    df[which.max(df$fracSurveys), "julianday"] # Day of max peak in % surveys with caterpillars
  })) %>%
  mutate(peakJunJul = map_dbl(meanDens, ~{
    df <- .
    commonTime <- filter(df, julianday >= 152 & julianday <= 212)
    commonTime[which.max(commonTime$fracSurveys), "julianday"] # Day of max peak in % surveys with caterpillars in June/July
  }))   %>%
  left_join(siteList) %>%
  left_join(unique(fullDataset[, c("Name", "Longitude")]))

## Plots 
theme_set(theme_bw())

# julian day labels
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

year <- data.frame(jds = jds, month = dates)
summer <- filter(year, jds > 100 & jds < 220)

# Maps of US and Canada
library(raster)
canada <- getData("GADM", country="CAN", level = 1)
usa <- getData("GADM", country = "USA", level = 1) # figure out how to remove water

ontario <- subset(canada, NAME_1 == "Ontario")
eastern_us <- subset(usa, NAME_1 %in% c("Michigan", "Indiana", "Kentucky", "Tennessee", "North Carolina", "Virginia",
                                        "West Virginia", "Ohio", "Maryland", "Delaware","New Jersey", "Pennsylvania",
                                        "New York", "Rhode Island", "Connecticut", "Massachusetts", "New Hampshire",
                                        "Vermont", "Maine", "South Carolina"))

# Plot of weighted mean peak in fraction of surveys with caterpillars by latitude, using only June and July
pdf('figs/weightedCaterpillarPeakScatter2018.pdf', height = 6, width = 8)
ggplot(t12_nested, aes(x = Latitude, y = weightedPeakJunJul)) + geom_point(aes(size = nRecs), col = "dodgerblue3") +
  labs(y = "Mean peak in caterpillars", size = "N. Surveys") + xlim(32, 49) +
  scale_y_continuous(breaks = summer$jds, labels = summer$month, limits = c(150, 215)) +
  scale_size(range = c(2, 7)) +
  theme(panel.grid = element_blank())
# add site names: geom_text(aes(x = Latitude + 1, y = weightedPeak - 2, label = Name))
dev.off()

# Map of same info
pdf('figs/weightedCaterpillarPeakMap2018.pdf', height = 8.5, width = 11)
ggplot() + geom_polygon(data = eastern_states, aes(x = long, y = lat, group = group), fill = "gray88", color = "white") + 
  geom_polygon(data = ontario, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  coord_map() +
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  geom_point(data = t12_nested, 
             aes(x = Longitude, y = Latitude, size = nRecs, color = weightedPeakJunJul)) +
  labs(x = NULL, y = NULL, size = "Number of surveys", color = "Mean peak in caterpillars") +
  ylim(31, 50) +
  scale_color_viridis(option = "C", breaks = summer$jds, labels = summer$month, limits = c(150, 215)) +
  scale_size(range=c(3, 6))
dev.off()

# Plot of peak date in June/July window
pdf('figs/CaterpillarPeakScatter2018.pdf', height = 6, width = 8)
ggplot(t12_nested, aes(x = Latitude, y = peakJunJul)) + geom_point(aes(size = nRecs), col = "dodgerblue3") +
  labs(y = "Peak in caterpillars", size = "N. Surveys") + xlim(32, 49) +
  scale_y_continuous(breaks = summer$jds, labels = summer$month, limits = c(150, 215)) +
  scale_size(range = c(2, 7)) +
  theme(panel.grid = element_blank())
# add site names: geom_text(aes(x = Latitude + 1, y = weightedPeak - 2, label = Name))
dev.off()

# Map of same info
pdf('figs/CaterpillarPeakMap2018.pdf', height = 8.5, width = 11)
ggplot() + geom_polygon(data = eastern_states, aes(x = long, y = lat, group = group), fill = "gray88", color = "white") + 
  geom_polygon(data = ontario, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  coord_map() +
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  geom_point(data = t12_nested, 
             aes(x = Longitude, y = Latitude, size = nRecs, color = peakJunJul)) +
  labs(x = NULL, y = NULL, size = "Number of surveys", color = "Peak in caterpillars") +
  ylim(31, 50) +
  scale_color_viridis(option = "C", breaks = summer$jds, labels = summer$month, limits = c(150, 215)) +
  scale_size(range=c(3, 6))
dev.off()