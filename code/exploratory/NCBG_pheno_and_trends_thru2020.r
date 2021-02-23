source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')



ncbg = filter(fullDataset, Name == "NC Botanical Garden", Year %in% c(2018, 2019, 2020))

bg18 = meanDensityByWeek(ncbg[ncbg$Year == 2018, ], ordersToInclude = 'caterpillar')
bg19 = meanDensityByWeek(ncbg[ncbg$Year == 2019, ], ordersToInclude = 'caterpillar')
bg20 = meanDensityByWeek(ncbg[ncbg$Year == 2020, ], ordersToInclude = 'caterpillar') %>% arrange(julianweek)

pdf('z:/projects/CaterpillarsCount/Results/NCBG_pheno_trends_thru2020.pdf', height = 6, width = 12)
par(mgp = c(3, 1, 0), cex.lab = 1.7, cex.axis = 1.25, mar = c(5, 5, 1, 1), mfrow = c(1, 2))
plot(bg18$julianweek, bg18$fracSurveys, type = 'l', lwd = 4, col = 'lightgreen', xlab = 'Day of year', 
     ylab = '% of surveys with caterpillars', las = 1, ylim = c(0, 16), xaxt = "n")
points(bg19$julianweek, bg19$fracSurveys, type = 'l', lwd = 4, col = 'forestgreen')
#points(bg20$julianweek, bg20$fracSurveys, type = 'l', lwd = 4, col = 'forestgreen')

jdAxis(jdRange = c(130, 214), biweekly = TRUE)

legend("topleft", legend = c("2018", "2019"), lwd = 4, col = c('lightgreen', 'forestgreen'), cex = 1.3)


annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, col = caterpillarCol,add = FALSE, ylim = c(0, .6), 
                main = "", plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'beetle', col = beetleCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'spider', col = spiderCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'leafhopper', col = hopperCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'truebugs', col = truebugCol,add = TRUE, plotVar = 'meanDensity')
legend("topright", legend = c("beetles", "spiders", "leafhoppers", "caterpillars", "true bugs"),
       col = c(beetleCol, spiderCol, hopperCol, caterpillarCol, truebugCol), lwd = 5, cex = 1.3)


dev.off()
