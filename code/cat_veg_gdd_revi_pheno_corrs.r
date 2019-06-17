# This file creates a vertically stacked set of caterpillar phenology curves
# based on Caterpillars Count! data from 2018 for an AGU poster

library(dplyr)
library(lubridate)
library(purrr)


source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# Read in sites
phenosites = read.table('data/sites_gdd_veg_bird_phenology.txt', header = T, sep = '\t', quote = '\"') 



# Caterpillar phenology metrics
summaryCC <- fullDataset %>%
  filter(Name %in% phenosites$Name, Year == 2018) %>%
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
  mutate(peakMayJun = map_dbl(meanDens, ~{
    df <- .
    commonTime <- filter(df, julianday >= 121 & julianday <= 181)
    commonTime[which.max(commonTime$fracSurveys), "julianday"] # Day of max peak in % surveys with caterpillars in June/July
  }))   %>%
  left_join(phenosites, by = 'Name') %>%
  arrange(desc(Latitude))

round(cor(summaryCC[,c('weightedPeak','weightedPeakJunJul','peak','peakJunJul','peakMayJun', 'hatching','gdd600jd','leafIndex')]),2)

cormat = round(cor(summaryCC[,c('gdd600jd','leafIndex','peakMayJun', 'hatching')]),2)


jdMin = 91
jdMax = 228

pdf('figs/caterpillar_phenology_and_metrics2018.pdf', height = 10, width = 10)
par(mfrow = c(ceiling(nrow(summaryCC)/2), 2), mar = c(0.3, 1, 1.3, 1), oma = c(5, 5, 0, 0))

i = 1

for (site in summaryCC$Name) {
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
  mtext(paste(site, ", ", round(summaryCC$Latitude[summaryCC$Name == site], 1), "°N;   ",
              summaryCC$nRecs[summaryCC$Name == site], "surveys", sep = ""), 3, line = 0, cex = .8)
  
  axis(1, at = jds, labels = F, tck = -.1)
  axis(1, at = jds + 14, labels = F, tck = -.05)
  
  # Shaded rectangles
  monthPos = 1:4
  
  rect(jds[monthPos[monthPos%%2 == 0]], rep(-10, length(monthPos[monthPos%%2 == 0])), 
       jds[monthPos[monthPos%%2 == 0] + 1]-1, rep(110, length(monthPos[monthPos%%2 == 0])), 
       col = rgb(.05, .05, .05, .03), border = NA)
  
  
  # Add reference lines
  abline(v = summaryCC$weightedPeak[summaryCC$Name == site], col = 'red', lwd = 3)
  abline(v = summaryCC$weightedPeakJunJul[summaryCC$Name == site], col = 'orange', lwd = 3)
  abline(v = summaryCC$peak[summaryCC$Name == site], col = 'lightgreen', lwd = 3)
  abline(v = summaryCC$peakJunJul[summaryCC$Name == site], col = 'lightblue', lwd = 3)
  abline(v = summaryCC$peakMayJun[summaryCC$Name == site], col = 'violet', lwd = 3)
  
  
  if (i == 1) {
    legend("topleft", legend = c("weightedPeak", 'weightedPeakJunJul', 'peak'),
           lwd = 3, col = c('red', 'orange', 'lightgreen'), bty = 'n')
    legend("topright", legend = c('peakJunJul', 'peakMayJun'), lwd = 3, col = c('lightblue', 'violet'), bty = 'n')
  }
  
  
  
  if (i <= 2 | i >= (nrow(summaryCC) - 1)) {
    text(jds, rep(0.5, length(jds)), dates, cex = .8)
  }
  i = i + 1
}
mtext(dates, 1, at = jds, cex = .7, line = .25)
mtext("Date", 1, outer = TRUE, line = 3, cex = 1.5)
mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 2.5, cex = 1.5)
dev.off()


# Plot of all pheno metrics vs latitude

pdf('figs/pheno_v_latitude.pdf', height = 8, width = 10)
par(mar = c(8, 10, 1, 1), mgp = c(4, 1, 0), cex.lab = 3, cex.axis = 2)
plot(summaryCC$Latitude, summaryCC$arrival, ylim = c(30,200), pch = 16, col = 'tomato', cex = 2,
     xlab = "Latitude", ylab = "", yaxt = 'n')
points(summaryCC$Latitude, summaryCC$leafIndex, pch = 16, cex = 2, col = 'lightgreen')
points(summaryCC$Latitude, summaryCC$gdd600jd, pch = 15, cex = 2, col = 'cornflowerblue')
points(summaryCC$Latitude, summaryCC$hatching, pch = 15, cex = 2, lty = 'dashed', col = 'tomato')
points(summaryCC$Latitude, summaryCC$peakMayJun, pch = 17, cex = 2, col = 'darkviolet')

axis(2, at = c(60,jds), c('Mar 1', dates), cex = 1.25, las = 1)
mtext("Date", 2, line = 7, cex = 3)

clip(min(summaryCC$Latitude), max(summaryCC$Latitude), 25, 200)
abline(latLeaf, col = 'lightgreen', lwd = 4)
abline(latGDD, col = 'cornflowerblue', lwd = 4)
abline(latCat, col = 'darkviolet', lwd = 4)
abline(latArrival, col = 'tomato', lwd = 4)
abline(latHatch, col = 'tomato', lwd = 4, lty = 'dashed')

text(42,190, expression(GDD[600]), col = 'cornflowerblue', srt = 20, cex = 2)
text(34.5,180, "Caterpillar\npeak", col = 'darkviolet', cex = 2)
text(42.5,90, "NPN First leaf", col = 'lightgreen', srt = 30, cex = 2)
text(39, 105, "Vireo arrival", col = 'tomato', srt = 10, cex = 2)
text(39, 142, "Vireo hatching (est)", col = 'tomato', srt = 10, cex = 1.5)
dev.off()
