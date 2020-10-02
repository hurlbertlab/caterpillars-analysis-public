# Caterpillar phenology at NC BOtanical Garden in 2018 and 2019 for book chapter.

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

ncbg = filter(fullDataset, Name == "NC Botanical Garden", Year %in% c(2018, 2019))

bg18 = meanDensityByWeek(ncbg[ncbg$Year == 2018, ], ordersToInclude = 'caterpillar')
bg19 = meanDensityByWeek(ncbg[ncbg$Year == 2019, ], ordersToInclude = 'caterpillar')

pdf('figs/caterpillars-count/NCBG_caterpillar_phenology_2018-19.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), cex.lab = 1.7, cex.axis = 1.25, mar = c(5, 5, 1, 1))
plot(bg18$julianweek, bg18$fracSurveys, type = 'l', lwd = 4, col = 'lightgreen', xlab = 'Julian day', 
     ylab = '% of surveys', las = 1, ylim = c(0, 16), xaxt = "n")
points(bg19$julianweek, bg19$fracSurveys, type = 'l', lwd = 4, col = 'forestgreen')
jdAxis(jdRange = c(130, 214), biweekly = TRUE)

cent18 = sum(bg18$fracSurveys*bg18$julianweek)/sum(bg18$fracSurveys)
cent19 = sum(bg19$fracSurveys*bg19$julianweek)/sum(bg19$fracSurveys)

peak18 = bg18$julianweek[bg18$fracSurveys == max(bg18$fracSurveys)]
peak19 = bg19$julianweek[bg19$fracSurveys == max(bg19$fracSurveys)]

abline(v = cent18, col = 'lightgreen', lwd = 3, lty = 'dashed')
abline(v = cent19, col = 'forestgreen', lwd = 3, lty = 'dashed')

arrows(c(peak18, peak19), c(16, 16), c(peak18, peak19), c(bg18$fracSurveys[bg18$julianweek == peak18], bg19$fracSurveys[bg19$julianweek == peak19]) + 0.2, col = c('lightgreen', 'forestgreen'), lwd = 3)

text(140, 3, "2018", cex = 2, col = 'lightgreen')
text(134, 8.5, "2019", cex = 2, col = 'forestgreen')

legend("topleft", legend = c("Centroid date", "Peak date"), lwd = c(3, 0), lty = c('dashed', 'blank'), bty = 'n', cex = 1.3)
arrows(129, 14.8, 134, 14.8, lwd = 3)

dev.off()
