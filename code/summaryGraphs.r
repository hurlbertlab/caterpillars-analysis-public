
source('code/summaryStats.r')
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


sites19 = read.table('data/revi/sitelist2019_revi.txt', header = TRUE, sep = '\t', quote='\"', stringsAsFactors = FALSE)

sites19_select12 = filter(sites19, Name %in% c('Sault College', 'Acadia NP - Alder', 'RVCC', 'Stage Nature Center', "Mass Audubon's Boston Nature Center",
                                            "Museum of American Bird Art", "Oregon Ridge Nature Center", "Walker Nature Center", 
                                            "Site A", "NC Botanical Garden", "Prairie Ridge Ecostation", "Fernbank Forest"))

multiSitePhenoPlot(fullDataset, 2019, sites19_select12, monthRange = c(5,8), REVI = TRUE, filename = 'caterpillarPhenology_12sites_2019', panelRows = 3, panelCols = 4)
