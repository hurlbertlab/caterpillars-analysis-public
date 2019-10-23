source('code/summaryStats.r')
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')
# source('code/phenology.r') #need siteList function


s15 = summaryStats(2015)
s16 = summaryStats(2016)
s17 = summaryStats(2017)
s18 = summaryStats(2018)
s19 = summaryStats(2019)

sall = rbind(as.data.frame(s15), as.data.frame(s16), as.data.frame(s17), as.data.frame(s18), as.data.frame(s19))

summ = cbind(data.frame(year = 2015:2019), sall)

dataset = fullDataset %>%
  filter(!grepl("BBS", Name), !grepl("Coweeta", Name), Name != "Example Site")

summ$numGoodSites = sapply(summ$year, function(yr) {
  sl = siteList(dataset, yr, minNumRecords = 100, minNumDates = 6, write = FALSE) %>%
    nrow()
})


par(mar = c(5, 5, 1, 1), mgp = c(3, 1, 0), cex.lab = 2, cex.axis = 1.25, las = 1)
plot(summ$year, summ$numSitesThisYear, type = 'l', lwd = 5, col = 'turquoise', xlab = 'Year', ylab = 'Number of sites')
points(summ$year, summ$numGoodSites, type = 'l', lwd = 5, col = 'orangered')
text(2018, 50, "Total sites", cex = 2, col = 'turquoise')
text(2018.6, 20, "High effort\nsites", cex = 2, col = 'orangered')

sites18 = read.table('data/revi/sitelist2018_revi.txt', header = TRUE, sep = '\t', quote='\"', stringsAsFactors = FALSE)
sites19 = read.table('data/revi/sitelist2019_revi.txt', header = TRUE, sep = '\t', quote='\"', stringsAsFactors = FALSE)

sites19_select12 = filter(sites19, Name %in% c('Sault College', 'Acadia NP - Alder', 'RVCC', 'Stage Nature Center', "Mass Audubon's Boston Nature Center",
                                               "Museum of American Bird Art", "Oregon Ridge Nature Center", "Walker Nature Center", 
                                               "Litzsinger Road Ecology Center Woodland Site A", "NC Botanical Garden", "Prairie Ridge Ecostation", "Fernbank Forest"))

multiSitePhenoPlot(fullDataset, 2019, sites19_select12, monthRange = c(5,8), REVI = TRUE, 
                   filename = 'caterpillarPhenology_12sites_2019', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3)

highEffortSites19 = siteEffortSummary(fullDataset, 2019) %>%
  filter(!is.na(medianGreenup),
         nSurveys >= 80,
         nGoodWeeks >= 5,
         #firstGDateAfterGreenup <=50,
         #lastGDateAfterGreenup >= 90,
         medianSurveysPerWeek > 10) %>%
  left_join(sites19[, c('Name', 'LatestWeekWithFreq0', 'WeekOfPeakFreq')], by = 'Name')

highEffortSites18 = siteEffortSummary(fullDataset, 2018) %>%
  filter(!is.na(medianGreenup),
         nSurveys >= 80,
         nGoodWeeks >= 5,
         #firstGDateAfterGreenup <=50,
         #lastGDateAfterGreenup >= 90,
         medianSurveysPerWeek > 10) %>%
  left_join(sites18[, c('Name', 'LatestWeekWithFreq0', 'WeekOfPeakFreq')], by = 'Name')

siteEffort17 = siteEffortSummary(fullDataset, 2017)

highEffortSites17 = siteEffort17 %>%
  filter(!is.na(medianGreenup),
         nSurveys >= 80,
         nGoodWeeks >= 5,
         #firstGDateAfterGreenup <=50,
         #lastGDateAfterGreenup >= 90,
         medianSurveysPerWeek > 10) 



# Caterpillar Phenology of High Effort Sites (all caterpillars)
multiSitePhenoPlot(fullDataset, 2019, highEffortSites19, monthRange = c(4,8), REVI = FALSE, minLength = 10,
                   filename = 'caterpillarPhenology_highEffortSites_2019_allCats_10+mm', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2019, highEffortSites19, monthRange = c(4,8), REVI = FALSE, minLength = 10, 
                   filename = 'caterpillarPhenology_highEffortSites_2019_bothCats_10+mm', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'both', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2018, highEffortSites18, monthRange = c(4,8), REVI = FALSE, minLength = 10,
                   filename = 'caterpillarPhenology_highEffortSites_2018_bothCats_10+mm', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'both', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2018, highEffortSites18, monthRange = c(4,8), REVI = FALSE, minLength = 10,
                   filename = 'caterpillarPhenology_highEffortSites_2018_allCats_10+mm', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2019, highEffortSites19, monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2019_bothCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'both', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2019, highEffortSites19, monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2019_allCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2018, highEffortSites18, monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2018_allCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2017, siteEffortSummary(fullDataset, 2017), monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2017_allCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2016, siteEffortSummary(fullDataset, 2016), monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2016_allCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

multiSitePhenoPlot(fullDataset, 2015, siteEffortSummary(fullDataset, 2015), monthRange = c(4,8), REVI = FALSE, plotVar = 'meanBiomass',
                   filename = 'caterpillarPhenology_highEffortSites_2015_allCats_biomass', panelRows = 3, panelCols = 4,
                   cex.axis = 1, cex.text = 1.5, cex.main = 1.3, whichCatLines = 'all', greenup = TRUE)

