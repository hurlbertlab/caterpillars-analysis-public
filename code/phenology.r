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

pheno19 = filter(fullPhenoSummary, Year == 2019, 
                 #numWeeksPostGreenupWindow >= 3,
                 numWeeksPostSolsticeWindow >= 3) %>%
  left_join(sites[, c('Name', 'Longitude', 'Latitude')], by = 'Name')

# Function for rescaling 
rescale = function(vec, newMin, newMax) {
  maxVec = max(vec, na.rm = T)
  minVec = min(vec[vec != -Inf], na.rm = T)
  newVec = newMin + (newMax - newMin)*(vec - minVec)/(maxVec - minVec)
  newVec[newVec == -Inf] = NA
  return(newVec)
}

colorScale = function(vec) {
  
  vec[vec == -Inf] = NA
  shades <- rainbow(130)[100:1]
  percents <- as.integer(cut(vec, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  return(fills)
}

# Function for generating 5 evenly spaced values from min to max for a variable
legendVals = function(vec) {
  vec[vec == -Inf] = NA
  maxvar = max(vec, na.rm = TRUE)
  minvar = min(vec, na.rm = TRUE)
  inc = (maxvar - minvar) / 4
  legend.text = c(minvar, minvar + inc, minvar + 2 * inc, minvar + 3 * inc, maxvar)
  return(legend.text)
}


# MAPS

NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')
pdf('figs/biomassPostGU_map.pdf', height = 8, width = 8)
plot(NAmap, xlim = c(-90, -70), ylim = c(28, 48), border = 'gray50', col = 'gray90')
points(pheno19$Longitude, pheno19$Latitude, pch = 16, col = colorScale(log10(pheno19$massPostGU)), 
       cex = 3 )

legend_image <- as.raster(matrix(rainbow(130)[1:100], ncol=1))
rasterImage(legend_image, -72, 30, -70, 36)
text(-70, 38, "Biomass\n(mg / survey)", cex = 1.3)
text(x = -68, y = seq(30, 36, length=5), labels = substr(10^legendVals(log10(pheno19$massPostGU)), 1, 5))
dev.off()


pdf('figs/biomassPostSolstice_map.pdf', height = 8, width = 8)
plot(NAmap, xlim = c(-90, -70), ylim = c(28, 48), border = 'gray50', col = 'gray90')
points(pheno19$Longitude, pheno19$Latitude, pch = 16, col = colorScale(log10(pheno19$massSolstice)), 
       cex = 3)

legend_image <- as.raster(matrix(rainbow(130)[1:100], ncol=1))
rasterImage(legend_image, -72, 30, -70, 36)
text(-70, 38, "Biomass\n(mg / survey)", cex = 1.3)
text(x = -68, y = seq(30, 36, length=5), labels = substr(10^legendVals(log10(pheno19$massSolstice)), 1, 5))
dev.off()


# Two example sites, cat biomass by plant species

#EwA at Fells and Prairie Ridge

fellsSurveys = filter(fullDataset, Name == "EwA at the Fells") %>%
  summarize(n = n_distinct(ID)) %>%
  pull(n)
  
prSurveys = filter(fullDataset, Name == "Prairie Ridge Ecostation") %>%
  summarize(n = n_distinct(ID)) %>%
  pull(n)

fellsCats = filter(fullDataset, Name == "EwA at the Fells", Group == 'caterpillar') %>%
  group_by(PlantSpecies) %>%
  summarize(totalBiomass = sum(Quantity*Biomass_mg),
            meanBiomass = totalBiomass/fellsSurveys) %>%
  arrange(desc(meanBiomass))

prCats = filter(fullDataset, Name == "Prairie Ridge Ecostation", Group == 'caterpillar') %>%
  group_by(PlantSpecies) %>%
  summarize(totalBiomass = sum(Quantity*Biomass_mg),
            meanBiomass = totalBiomass/prSurveys) %>%
  arrange(desc(meanBiomass))

pdf('figs/cats_by_treesp_examples.pdf', height = 8, width = 8)
par(mar= c(8, 6, 1, 1), mgp = c(2.5, 1, 0), mfrow = c(2, 1))
b1 = barplot(log10(fellsCats$meanBiomass)+1, yaxt = "n", ylim = c(0, 3), col = 'red')
axis(2, at = seq(0, 3, by = 0.5), labels = round(10^(seq(-1, 2, by = 0.5)), 1), las = 1)
text(b1, rep(-.1, 8), fellsCats$PlantSpecies, srt=45, adj = 1, xpd = TRUE, cex = 1.2)
mtext("Biomass (mg / survey)", 2, cex = 1.5, line = 3.5)

b2 = barplot(log10(prCats$meanBiomass[1:8])+1.25, yaxt = "n", ylim = c(0, 2.75), col = rgb(157/255, 1, 0))
axis(2, at = seq(0.25, 2.75, by = 0.5), labels = round(10^(seq(-1, 1.5, by = 0.5)), 1), las = 1)
text(b1, rep(-.1, 8), prCats$PlantSpecies[1:8], srt=45, adj = 1, xpd = TRUE, cex = 1.2)
mtext("Biomass (mg / survey)", 2, cex = 1.5, line = 3.5)
dev.off()


monthRange = c(4,8)
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# x-axis range
if (is.null(monthRange)) {
  # make sure xlim endpoints coincide with month labels
  if(length(unique(sitedata$julianday)) == 1) {
    minPos = which(jds == max(jds[jds <= min(sitedata$julianday)]))
    maxPos = which(jds == min(jds[jds >= max(sitedata$julianday)]))
  } else {
    minPos = max(which(jds == min(jds[jds >= min(sitedata$julianday)])) - 1, 1)    
    maxPos = min(which(jds == max(jds[jds <= max(sitedata$julianday)])) + 1, 12)
  }
} else {
  minPos = monthRange[1]
  maxPos = monthRange[2]+1
}
monthLabs = minPos:(maxPos-1)


# Phenology at Prairie Ridge
#REVI phenology
wake_revi2019 = readEbirdBarchart(path = 'data/revi', countyCode = 'US-NC-183') 
matedate = wake_revi2019$julianday[wake_revi2019$julianday == max(wake_revi2019$julianday[wake_revi2019$freq > .9*max(wake_revi2019$freq)])]

pr19 = filter(fullDataset, Year==2019, Name == "Prairie Ridge Ecostation")

pdf('figs/PrairieRidge_2019_cat_biomass.pdf', height = 5, width = 7)
par(mar = c(4, 5, 1, 4))
meanDensityByWeek(pr19, ordersToInclude ='caterpillar', plotVar = 'meanBiomass', 
                  allDates = F, plot = TRUE, col = 'purple3', lwd = 4, xlim = c(91, 210), 
                  xaxt = 'n', xlab = '', ylab = 'Caterpillar biomass (mg / survey)', xaxs = 'i',
                  cex.lab = 1.5, cex.axis = 1.3, new = TRUE)
mtext(dates[monthLabs[1:(length(monthLabs)-1)]], 1, at = jds[monthLabs[1:(length(monthLabs)-1)]]+14, cex = 1.5, line = .4)
abline(v = jds, col = 'gray80')

meanDensityByWeek(pr19, ordersToInclude ='caterpillar', plotVar = 'meanBiomass', 
                  allDates = F, plot = TRUE, col = 'purple3', lwd = 4, xlim = c(91, 210), 
                  xaxt = 'n', xlab = '', ylab = '', xaxs = 'i',
                  cex.lab = 1.5, cex.axis = 1.3, new = FALSE)
abline(v = 91)
dev.off()

pdf('figs/PrairieRidge_2019_cat_biomass_REVIpheno.pdf', height = 5, width = 7)
par(mar = c(4, 5, 1, 4))
plot(wake_revi2019$julianday, wake_revi2019$freq, xaxt = "n", yaxt = "n", type = 'n',
     lwd = 2, col = 'salmon', xlab = '', ylab = '', xlim = c(91, 210), xaxs = 'i')
mtext("Red-eyed Vireo frequency", 4, col = 'salmon', line = 1, cex = 1.5)
mtext(dates[monthLabs[1:(length(monthLabs)-1)]], 1, at = jds[monthLabs[1:(length(monthLabs)-1)]]+14, cex = 1.5, line = .4)
abline(v = jds, col = 'gray80')

# REVI
points(wake_revi2019$julianday, wake_revi2019$freq, type = 'l', lwd = 2, col = 'salmon')

par(new = TRUE)

meanDensityByWeek(pr19, ordersToInclude ='caterpillar', plotVar = 'meanBiomass', 
                  allDates = F, plot = TRUE, col = 'purple3', lwd = 4, xlim = c(91, 210), 
                  xaxt = 'n', xlab = '', ylab = 'Caterpillar biomass (mg / survey)', xaxs = 'i',
                  cex.lab = 1.5, cex.axis = 1.3, new = TRUE)
dev.off()

pdf('figs/PrairieRidge_2019_cat_biomass_REVInestlings.pdf', height = 5, width = 7)
par(mar = c(4, 5, 1, 4))
plot(wake_revi2019$julianday, wake_revi2019$freq, xaxt = "n", yaxt = "n", type = 'n',
     lwd = 2, col = 'salmon', xlab = '', ylab = '', xlim = c(91, 210), xaxs = 'i')
mtext("Red-eyed Vireo frequency", 4, col = 'salmon', line = 1, cex = 1.5)
mtext(dates[monthLabs[1:(length(monthLabs)-1)]], 1, at = jds[monthLabs[1:(length(monthLabs)-1)]]+14, cex = 1.5, line = .4)
abline(v = jds, col = 'gray80')

# Show estimated nestling period, 24-36 days post mate-finding (which is when singing is assumed to drop off)
# -- 5d nest building + 2d pre-laying + 4d laying + 13d incubation + 12d nestlings
rect(matedate + 24, 0, matedate + 24 + 12, 1, col = 'mistyrose', border = NA)
points(wake_revi2019$julianday, wake_revi2019$freq, type = 'l', lwd = 2, col = 'salmon')

par(new = TRUE)

meanDensityByWeek(pr19, ordersToInclude ='caterpillar', plotVar = 'meanBiomass', 
                  allDates = F, plot = TRUE, col = 'purple3', lwd = 4, xlim = c(91, 210), 
                  xaxt = 'n', xlab = '', ylab = 'Caterpillar biomass (mg / survey)', xaxs = 'i',
                  cex.lab = 1.5, cex.axis = 1.3, new = TRUE)
dev.off()



# Select 2019 sites
sites19 = read.csv('data/revi/sitelist2019_revi.csv', header = TRUE, quote='\"', stringsAsFactors = FALSE)

sites19_select10 = filter(sites19, Name %in% c("RVCC", "Stage Nature Center", "Mass Audubon's Boston Nature Center",
                                               "Walker Nature Center", "Potter Park Zoo", "Riverbend Park", "Georgetown",
                                                "NC Botanical Garden", "Prairie Ridge Ecostation", "Fernbank Forest"))

multiSitePhenoPlot(fullDataset, 2019, sites19_select10, monthRange = c(4,8), REVI = 'arrivaldate', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_10sites_2019', panelRows = 2, panelCols = 5,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =6, width = 12, colREVI = rgb(1, 228/255, 225/255))


multiSitePhenoPlot(fullDataset, 2019, sites19full, monthRange = c(4,8), REVI = 'matedate', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_allSites_2019_REVImatedate_greenup', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =8.5, width = 11, colREVI = rgb(1, 228/255, 225/255))


### Comparison of REVI phenology across sites
revi_output = data.frame(Name = NA, matedate = NA)

# threshold date for calculating peak bird occurrence should vary with latitude
# at 32 deg N, threshold should be 150, at 45 deg N threshold should be 210; 
# threshold = 4.615*latitude + 2.308
latitudeBasedJDthreshold = function(latitude) {
  jd = 4.615*latitude + 2.308
  return(jd)
}

# Calculates the last date within a seasonal window that varies by latitude 
# (according to the latitudeBasedJDthreshold; later window at higher latitudes)
# for which the observed bird frequency is within 0.9 (or other specified proportion)
# of the maximum frequency in that window. Thus, dips in frequency are ignored if
# frequency comes back up close to the max. This date really captures the period
# during which there is a pretty steep drop off in frequency.

matedateCalc1 = function(birdFreqDataframe, latitude, proportionOfPeak = 0.9) {
  matedate = birdFreqDataframe$julianday[birdFreqDataframe$julianday == 
                          max(birdFreqDataframe$julianday[birdFreqDataframe$freq > .proportionOfPeak*max(birdFreqDataframe$freq[birdFreqDataframe$julianday < latitudeBasedJDthreshold(latitude)]) & 
                                                        birdFreqDataframe$julianday < latitudeBasedJDthreshold(latitude)])]
  return(matedate)
}

# Calculates the first date at which observed bird frequency drops by more than 0.1
# (or other specified proportion) times the maximum frequency. Frequency might rebound
# back up to close to the maximum value before dropping off steeply, but it is assumed that
# the first "big" dip is the period we want to characterize.
matedateCalc2 = function(birdFreqDataframe, dipFromPeak = 0.1) {
  freqDiff = diff(birdFreqDataframe$freq)
  diffRelativeToMax = freqDiff/max(birdFreqDataframe$freq, na.rm = TRUE)
  firstIndex = min(which(diffRelativeToMax < -dipFromPeak))
  return(birdFreqDataframe$julianday[firstIndex])
}


pdf('figs/REVI_phenology_2019_allSites_matedate2.pdf', height = 6, width = 12)
par(mfrow = c(2, 5), mar = c(4, 4,2, 1), oma = c(4, 4 , 0, 0))
for (s in sites19$Name) {
  temp = readEbirdBarchart('data/revi', countyCode = sites19$ebirdCounty[sites19$Name == s])
  latitude = sites19$Latitude[sites19$Name == s]
  plot(temp$julianday, temp$freq, type = 'l', lwd = 2, col = 'limegreen', xlab = '', ylab = '', main = s)
  matedate = matedateCalc2(temp, dipFromPeak = .1)
  abline(v = matedate)
  legend("topright", legend = paste0(round(sites19$Latitude[sites19$Name == s], 1), "°N"), bty = 'n')
  
  revi_output = rbind(revi_output, data.frame(Name = s, matedate = matedate))
  
}
mtext('Red-eyed Vireo frequency', 2, outer = TRUE, cex = 1.5)
mtext('Julian day', 1, outer = TRUE, cex = 1.5)
dev.off()

write.csv(revi_output, 'data/revi/revi_matedate_2019_allsites.csv', row.names = F)

sites19md = read.csv('data/revi/revi_matedate_2019_allsites.csv', header = T)

sites19full = left_join(sites19, revi_output, by = 'Name') %>% 
  filter(nSurvs > 80)

multiSitePhenoPlot(fullDataset, 2019, sites19full, monthRange = c(4,8), REVI = 'matedate', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                                       filename = 'caterpillarPhenology_allSites_2019_REVImatedate_greenup', panelRows = 3, panelCols = 4,
                                       cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =8.5, width = 11, colREVI = rgb(1, 228/255, 225/255))











# Top 12 sites without ECU, weighted mean fraction of surveys with caterpillars


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