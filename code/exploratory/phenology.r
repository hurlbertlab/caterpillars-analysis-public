# Script for displaying caterpillar phenology across sites

library(dplyr)
library(lubridate)
library(sp)
library(maps)
library(maptools)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# Read in environmental data
envData = read.csv('data/env/npn_gdd_firstleaf.csv', header = T)

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
fullPhenoSummary = phenoSummary(fullDataset, ordersToInclude = 'caterpillar', postGreenupBeg = 30, postGreenupEnd = 75, minNumWeeks = 5) %>%
  left_join(envData[, -which(names(envData)=='medianGreenup')], by = c('Name', 'Year'))


# Coweeta NPN green up vs caterpillar biomass peak
pdf('figs/Coweeta_massPeakDateWindow_v_date2000GDD.pdf', height = 6, width = 8)
par(mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), cex.axis = 1.3, cex.lab = 1.8, oma = c(0, 0, 0, 0))
plot(fullPhenoSummary$date2000GDD[grepl("Coweeta", fullPhenoSummary$Name)], 
     fullPhenoSummary$massPeakDateWindow[grepl("Coweeta", fullPhenoSummary$Name)], 
     pch = 16, col = 'darkgreen', cex = 2.5, las = 1, xlab = "Date of accumulation of 2000 GDD",
     ylab = "Caterpillar Biomass Peak Date")
massPeakDateWindow_gddDate_lm = lm(massPeakDateWindow ~ date2000GDD, data = fullPhenoSummary[grepl("Coweeta", fullPhenoSummary$Name),])
abline(massPeakDateWindow_gddDate_lm, lwd = 2)
text(190, 200, expression(paste(R^2, "= 0.21")), cex = 1.3)
text(190, 195, expression(paste(italic(p), "= 0.046")), cex = 1.3)
dev.off()


pdf('figs/Coweeta_massPeakDateWindow_v_npn_firstleaf.pdf', height = 6, width = 8)
par(mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), cex.axis = 1.3, cex.lab = 1.8, oma = c(0, 0, 0, 0))
plot(fullPhenoSummary$npn_firstLeaf[grepl("Coweeta", fullPhenoSummary$Name)], 
     fullPhenoSummary$massPeakDateWindow[grepl("Coweeta", fullPhenoSummary$Name)], 
     pch = 16, col = 'darkgreen', cex = 2.5, las = 1, xlab = "Date of accumulation of 2000 GDD",
     ylab = "Caterpillar Biomass Peak Date")
massPeakDateWindow_leaf_lm = lm(massPeakDateWindow ~ npn_firstLeaf, data = fullPhenoSummary[grepl("Coweeta", fullPhenoSummary$Name),])
abline(massPeakDateWindow_leaf_lm, lwd = 2)
text(190, 200, expression(paste(R^2, "= 0.21")), cex = 1.3)
text(190, 195, expression(paste(italic(p), "= 0.046")), cex = 1.3)
dev.off()



pheno19 = filter(fullPhenoSummary, Year == 2019, 
                 #numWeeksPostGreenupWindow >= 3,
                 numWeeksPostSolsticeWindow >= 3)

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


pdf('figs/pctSurveysPostSolstice_map.pdf', height = 8, width = 8)
plot(NAmap, xlim = c(-90, -70), ylim = c(28, 48), border = 'gray50', col = 'gray90')
points(pheno19$Longitude, pheno19$Latitude, pch = 16, col = colorScale(pheno19$pctSolstice), 
       cex = 3)

legend_image <- as.raster(matrix(rainbow(130)[1:100], ncol=1))
rasterImage(legend_image, -72, 30, -70, 36)
text(-70, 38, "% of surveys\nw/ caterpillars", cex = 1.3)
text(x = -69, y = seq(30, 36, length=5), labels = round(legendVals(pheno19$pctSolstice)))
dev.off()


#####################################################
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
sites19 = siteSummary(fullDataset, 2019, write = FALSE) 

### Comparison of REVI phenology across sites
year = 2019

revi_output = data.frame(Name = NA, Year = year, matedate1 = NA, matedate2 = NA)


pdf('figs/REVI_phenology_2019_allSites_matedate1.pdf', height = 6, width = 12)
par(mfrow = c(2, 5), mar = c(4, 4,2, 1), oma = c(4, 4 , 0, 0))
for (s in sites19$Name) {
  temp = readEbirdBarchart('data/revi', countyCode = sites19$ebirdCounty[sites19$Name == s],
                           yearBeg = year, yearEnd = year)
  latitude = sites19$Latitude[sites19$Name == s]
  plot(temp$julianday, temp$freq, type = 'l', lwd = 2, col = 'limegreen', xlab = '', ylab = '', main = s)
  matedate1 = matedateCalc1(temp, latitude, proportionOfPeak = .9)
  matedate2 = matedateCalc2(temp, dipFromPeak = .1)
  abline(v = matedate1)
  legend("topright", legend = paste0(round(sites19$Latitude[sites19$Name == s], 1), "°N"), bty = 'n')
  
  revi_output = rbind(revi_output, data.frame(Name = s, Year = year, matedate1 = matedate1, matedate2 = matedate2))
  
}
mtext('Red-eyed Vireo frequency', 2, outer = TRUE, cex = 1.5)
mtext('Julian day', 1, outer = TRUE, cex = 1.5)
dev.off()

revi_output = revi_output[-1,]

write.csv(revi_output, 'data/revi/revi_matedate_2019_allsites.csv', row.names = F)

revi_output = read.csv('data/revi/revi_matedate_2019_allsites.csv', header = T, stringsAsFactors = F)

sites19full = sites19 %>%
  left_join(revi_output, by = 'Name') %>%
  filter(nSurveys > 80)

sites19_select10 = filter(sites19full, Name %in% c("RVCC", "Stage Nature Center", "Mass Audubon's Boston Nature Center",
                                               "Walker Nature Center", "Potter Park Zoo", "Riverbend Park", "Georgetown",
                                               "NC Botanical Garden", "Prairie Ridge Ecostation", "Fernbank Forest"))

multiSitePhenoPlot(fullDataset, 2019, sites19full, monthRange = c(4,8), REVI = 'matedate2', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                                       filename = 'caterpillarPhenology_allSites_2019_REVImatedate2_greenup', panelRows = 3, panelCols = 4, greenup = T,
                                       cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =8.5, width = 11, colREVI = rgb(1, 228/255, 225/255))

# Select sites, new dimensions
multiSitePhenoPlot(fullDataset, 2019, sites19_select10, monthRange = c(4,8), REVI = 'matedate2', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_selectSites_2019_REVImatedate2_greenup', panelRows = 2, panelCols = 5, greenup = T,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =6, width = 12, colREVI = rgb(1, 228/255, 225/255))


multiSitePhenoPlot(fullDataset, 2019, sites19_select10, monthRange = c(4,8), REVI = 'matedate2', ordersToInclude = 'caterpillar', plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_selectSites_2019_REVImatedate2', panelRows = 2, panelCols = 5, greenup = F,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, height =6, width = 12, colREVI = rgb(1, 228/255, 225/255))




### Comparison of REVI phenology across years at NCBG and PR

pdf('figs/REVI_phenology_NCBG_PR_2015-2019.pdf', height = 6, width = 12)
par(mfcol = c(2, 5), mar = c(4, 4,2, 1), oma = c(4, 4 , 0, 0))
for (y in 2015:2019) {
  tempBG = getEbirdBarchartData(countyCode = 'US-NC-135', year = y)
  tempPR = getEbirdBarchartData(countyCode = 'US-NC-183', year = y)
  
  matedate2BG = matedateCalc2(tempBG, dipFromPeak = .1)
  matedate2PR = matedateCalc2(tempPR, dipFromPeak = .1)
  
  plot(tempBG$julianday, tempBG$freq, type = 'l', lwd = 2, col = 'limegreen', xlab = '', ylab = '', 
       main = paste0(y, ", ", matedate2BG))
  abline(v = matedate2BG)
  if (y == 2015) {
    legend("topright", "NCBG", bty = 'n', cex = 1.5)
  }
  
  plot(tempPR$julianday, tempPR$freq, type = 'l', lwd = 2, col = 'limegreen', xlab = '', ylab = '', 
       main = paste0(y, ", ", matedate2PR))
  abline(v = matedate2PR)
  if (y == 2015) {
    legend("topright", "PR", bty = 'n', cex = 1.5)
  }
  
}
mtext('Red-eyed Vireo frequency', 2, outer = TRUE, cex = 1.5)
mtext('Julian day', 1, outer = TRUE, cex = 1.5)
dev.off()







############################################
# Exploring caterpillar phenometrics

pheno19 = fullPhenoSummary %>%
  filter(Year == 2019, 
         numWeeksPostSolsticeWindow >= 3) %>%
  left_join(revi_output, by = c('Name', 'Year')) %>%
  left_join(sites[, c('Name', 'Longitude', 'Latitude')], by = 'Name')

pheno18 = fullPhenoSummary %>%
  filter(Year == 2018, 
         numWeeksPostSolsticeWindow >= 3) %>%
  #left_join(revi_output, by = c('Name', 'Year')) %>%
  left_join(sites[, c('Name', 'Longitude', 'Latitude')], by = 'Name')


# Get raster data of tmin
tmin = getData('worldclim', var = 'tmin', res = 2.5)
pheno19$Mar_MayTmin_normal = apply(extract(tmin, pheno19[, c('Longitude', 'Latitude')])[, 3:5], 1, mean)/10


# Linear model predicting peak mass date with greenup date and tmin

massModel1 = lm(massPeakDateWindow ~ medianGreenup + Mar_MayTmin_normal, data = pheno19)
massModel2 = lm(massPeakDate ~ medianGreenup + Mar_MayTmin_normal, data = pheno19)




pdf('figs/caterpillarPhenology_metrics_allSites_2019.pdf', height = 8.5, width = 11)
par(mfrow = c(3, 4), mar = c( 4, 4, 3, 1))
for (s in pheno19$Name) {
  tmp = filter(fullDataset, Name == s, Year == 2019)
  meanDensityByWeek(tmp, ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', plot = TRUE,
                    xlim = c(91, 211), col = 'purple3', lwd = 4, xlab = '', ylab = 'Mean Biomass',
                    main = siteNameForPlotting(s))
  abline(v = pheno19$massPeakDate[pheno19$Name == s], col = 'red', lwd = 6)
  abline(v = pheno19$massPeakDateWindow[pheno19$Name == s], col = 'blue', lwd = 3)
  abline(v = pheno19$massRollingPeakDateWindow[pheno19$Name == s], col= 'green', lwd = 1)
  
  if (s == pheno19$Name[1]) {
    legend("topleft", legend = c('peak', 'peakWindow', 'rollingPeak'), lwd = c(6,3,1), 
           col = c('red', 'blue', 'green', lty = 'solid'), bty = 'n')
  }
}
dev.off()


pdf('figs/caterpillarPhenology_metrics_allSites_2018.pdf', height = 8.5, width = 11)
par(mfrow = c(3, 4), mar = c( 4, 4, 3, 1))
for (s in pheno18$Name) {
  tmp = filter(fullDataset, Name == s, Year == 2018)
  meanDensityByWeek(tmp, ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', plot = TRUE,
                    xlim = c(91, 211), col = 'purple3', lwd = 4, xlab = '', ylab = 'Mean Biomass',
                    main = siteNameForPlotting(s))
  abline(v = pheno18$massPeakDate[pheno18$Name == s], col = 'red', lwd = 6)
  abline(v = pheno18$massPeakDateWindow[pheno18$Name == s], col = 'blue', lwd = 3)
  abline(v = pheno18$massRollingPeakDateWindow[pheno18$Name == s], col= 'green', lwd = 1)
  
  if (s == pheno18$Name[1]) {
    legend("topleft", legend = c('peak', 'peakWindow', 'rollingPeak'), lwd = c(6,3,1), 
           col = c('red', 'blue', 'green', lty = 'solid'), bty = 'n')
  }
}
dev.off()


###################################################
# NC Botanical garden thru time
ncbg = fullDataset %>%
  filter(Name == "NC Botanical Garden") 

pdf('figs/NCBG_caterpillar_biomass_pheno_2015.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015, lwd = 5, col = c('purple2'), bty = 'n')
dev.off()


pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2016.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2016, lwd = 5, col = c('purple2', 'blue'), bty = 'n')
dev.off()


pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2017.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2017, lwd = 5, col = c('purple2', 'blue', 'skyblue'), bty = 'n')
dev.off()


pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2018.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2018, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink'), bty = 'n')
dev.off()



pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2019.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2019, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon'), bty = 'n')
dev.off()




pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2020.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_20_biomass = ncbg %>%
  filter(Year == 2020) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'red', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2020, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon', 'red'))
dev.off()



pdf('figs/NCBG_caterpillar_biomass_pheno_2015-2021.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_20_biomass = ncbg %>%
  filter(Year == 2020) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'red', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_21_biomass = ncbg %>%
  filter(Year == 2021) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'darkred', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2021, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon', 'red', 'darkred'))
dev.off()





pdf('figs/NCBG_caterpillar_biomass_pheno_2018-2020.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'lightpink', new = TRUE,
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 3))

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_20_biomass = ncbg %>%
  filter(Year == 2020) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanBiomass', col = 'darkred', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2018:2020, lwd = 5, col = c('lightpink', 'salmon', 'darkred'))
dev.off()



pdf('figs/NCBG_caterpillar_density_pheno_2018-2020.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanDensity', col = 'lightpink', new = TRUE,
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, .2))

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanDensity', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)

ncbg_20_biomass = ncbg %>%
  filter(Year == 2020) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'meanDensity', col = 'darkred', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5)
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillars per survey", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2018:2020, lwd = 5, col = c('lightpink', 'salmon', 'darkred'))
dev.off()







pdf('figs/NCBG_caterpillar_occurrence_pheno_2015-2019.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
ncbg_15_biomass = ncbg %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = FALSE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 25))

ncbg_16_biomass = ncbg %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = FALSE, lwd = 5)

ncbg_17_biomass = ncbg %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = FALSE, lwd = 5)

ncbg_18_biomass = ncbg %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = FALSE, lwd = 5)

ncbg_19_biomass = ncbg %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = FALSE, lwd = 5)

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar occurrence (% surveys)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2019, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon'))
dev.off()





# Grace's analysis
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