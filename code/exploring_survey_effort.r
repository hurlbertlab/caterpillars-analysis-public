# Exploring phenological variation within regions

library(dplyr)
library(lubridate)
library(sp)
library(maps)
library(maptools)
library(tidyr)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

aggregatedSites = read.csv('data/aggregatedSites.csv', header = T, quote = '\"', stringsAsFactors = F)

dataset = fullDataset %>%
  left_join(aggregatedSites[, c('Name', 'AggregatedSite')], by = 'Name')

boston = dataset %>%
  filter(AggregatedSite == 'Boston')

aggregateComponentPlot(boston, ylim = c(0,50), allCats = F)
aggregateComponentPlot(boston, ylim = c(0,100), allCats = T)




highEffortSites = filter(ss, 
                         nSurveys >= 80,
                         nGoodWeeks >= 5,
                         firstGoodDate <=150,
                         lastGoodDate >= 180,
                         medianEffortDeviation <= 10)


# Plotting number of surveys per week at each site
sspw = siteSurveysPerWeek(fullDataset, 2019)
par(mar = c(4, 15, 1, 1))
plot(c(80, 270), c(1, nrow(sspw)), type = 'n', xlab = 'Julian day', ylab = '', yaxt = 'n')
sapply(1:nrow(sspw), function(x) points(as.numeric(names(sspw)[2:ncol(sspw)]), rep(x, ncol(sspw)-1), 
                                cex = unlist(log(sspw[x, 2:ncol(sspw)]))/2, pch = 16, 
                                col = rainbow(nrow(sspw))[x]))
mtext(substr(sspw$Name, 1, 40), 2, at = 1:nrow(sspw), las = 1, line = 1, cex = .8)
