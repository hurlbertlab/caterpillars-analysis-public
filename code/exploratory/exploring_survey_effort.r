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



# Seasonal distribution of effort across sites

# Plotting number of surveys per week at each site
sspw = siteSurveysPerWeek(fullDataset, 2019)
par(mar = c(4, 15, 1, 1))
plot(c(80, 270), c(1, nrow(sspw)), type = 'n', xlab = 'Julian day', ylab = '', yaxt = 'n')
sapply(1:nrow(sspw), function(x) points(as.numeric(names(sspw)[2:ncol(sspw)]), rep(x, ncol(sspw)-1), 
                                cex = unlist(log(sspw[x, 2:ncol(sspw)]))/3, pch = 16, 
                                col = rainbow(nrow(sspw))[x]))
mtext(substr(sspw$Name, 1, 40), 2, at = 1:nrow(sspw), las = 1, line = 1, cex = .8)

# Same as above but relative to green up
sspwgu = siteSurveysPerWeek(fullDataset, 2019, relativeToGreenup = TRUE)
par(mar = c(4, 15, 1, 1))
plot(range(as.numeric(names(sspwgu)[c(2:ncol(sspwgu))]), na.rm = TRUE), c(1, nrow(sspwgu)), type = 'n', xlab = 'Days after median green up', ylab = '', yaxt = 'n')
sapply(1:nrow(sspwgu), function(x) points(as.numeric(names(sspwgu)[2:ncol(sspwgu)]), rep(x, ncol(sspwgu)-1), 
                                        cex = unlist(log(sspwgu[x, 2:ncol(sspwgu)]))/3, pch = 16, 
                                        col = rainbow(nrow(sspwgu))[x]))
mtext(substr(sspwgu$Name, 1, 40), 2, at = 1:nrow(sspwgu), las = 1, line = 1, cex = .8)


# Plotting range of "good dates" relative to median green up date
ses = siteEffortSummary(fullDataset, 2019)
ses2 = ses[!is.na(ses$medianGreenup),]
par(mar = c(4, 15, 1, 1))
plot(c(min(ses$firstGDateAfterGreenup, na.rm = T), max(ses$lastGDateAfterGreenup, na.rm = T)), c(1,nrow(ses2)), yaxt = "n", xlab = "Days after greenup", ylab = "", type = 'n')
sapply(1:nrow(ses2), function(x) segments(ses2$firstGDateAfterGreenup[x], x, ses2$lastGDateAfterGreenup[x], x))
mtext(substr(ses2$Name, 1, 40), 2, at = 1:nrow(ses2), las = 1, line = 1, cex = .8)
abline(v = c(40, 95), col = 'red')

# Plotting for subset of "high effort sites"
highEffortSites = filter(ses2, 
                         nSurveys >= 80,
                         nGoodWeeks >= 5,
                         firstGDateAfterGreenup <=50,
                         lastGDateAfterGreenup >= 90,
                         medianEffortDeviation <= 10)


