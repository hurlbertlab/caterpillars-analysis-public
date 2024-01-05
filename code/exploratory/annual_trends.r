# Script for displaying annual trends in caterpillars or other arthropod groups
options(stringsAsFactors = FALSE)
source('code/reading_datafiles_without_users.r')

annualTrendPlot = function(fullDataset, 
                           site, 
                           jdRange = c(130, 210),
                           plotVar = "fracSurveys",    #other option "meanDensity"
                           group = "caterpillar", 
                           minSurveysPerYear = 100,
                           add = FALSE,
                           ...) {
  
  df = filter(fullDataset, Name == site, julianday >= jdRange[1], julianday <= jdRange[2])
  
  survsPerYear = df %>%
    distinct(Year, ID) %>%
    count(Year) %>%
    rename(totalSurvs = n) 
  
  output = df %>%
    filter(Group %in% group) %>%
    group_by(Year) %>%
    summarize(nSurvs = sum(Quantity > 0, na.rm = T),
              nTot = sum(Quantity, na.rm = T)) %>%
    left_join(survsPerYear, by = 'Year') %>%
    mutate(fracSurveys = 100*nSurvs/totalSurvs,
           meanDensity = nTot/totalSurvs) %>%
    filter(totalSurvs >= minSurveysPerYear) %>%
    data.frame()
  
  if (plotVar == "fracSurveys") { 
    yLabel = "Percent of surveys"
  } else {
    yLabel = "Mean number per survey"
  }
  
  if (add) {
    points(output$Year, output[, plotVar], type = 'l', las = 1, ylab = yLabel, xlab = "", ...)
  } else {
    plot(output$Year, output[, plotVar], type = 'l', las = 1, ylab = yLabel, xlab = "", ...)
  }
}

barColors = rev(c("#222222", "#000dff", "#00eeff", "#039e0d", "#00ff11", "#f7ff00", "#ff8c00", "#ff0000", "#96039e", "#f200ff", "#fa7d7d", "#fcc37e", "#bbbbbb", "#7f85ff"))
spiderCol = barColors[11] 
beetleCol = barColors[4]
truebugCol = barColors[12]
hopperCol = barColors[9]
caterpillarCol = barColors[5]

# Plots

# 2-panel, PR and NCBG
pdf('figs/NCBG_PRE_annual_trends_2015-2020_density.pdf', height = 4, width = 7)
par(mfrow = c(1, 2), mar = c(3, 4, 3, 1), mgp = c(2.5, 1, 0), oma = c(0,0,0,0))
annualTrendPlot(fullDataset, "Prairie Ridge Ecostation", lwd = 5, col = caterpillarCol,add = FALSE, ylim = c(0, .8), 
                main = "Prairie Ridge Ecostation", plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "Prairie Ridge Ecostation", lwd = 5, group = 'beetle', col = beetleCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "Prairie Ridge Ecostation", lwd = 5, group = 'spider', col = spiderCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "Prairie Ridge Ecostation", lwd = 5, group = 'leafhopper', col = hopperCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "Prairie Ridge Ecostation", lwd = 5, group = 'truebugs', col = truebugCol,add = TRUE, plotVar = 'meanDensity')

annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, col = caterpillarCol,add = FALSE, ylim = c(0, .6), 
                main = "NC Botanical Garden", plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'beetle', col = beetleCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'spider', col = spiderCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'leafhopper', col = hopperCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'truebugs', col = truebugCol,add = TRUE, plotVar = 'meanDensity')
legend("topright", legend = c("beetles", "spiders", "leafhoppers", "caterpillars", "true bugs"),
       col = c(beetleCol, spiderCol, hopperCol, caterpillarCol, truebugCol), lwd = 5, cex = .8)
dev.off()



# NCBG only
pdf('figs/NCBG_annual_trends_2015-2020_density.pdf', height = 4, width = 6)
par(mfrow = c(1, 1), mar = c(3, 4, 3, 1), mgp = c(2.5, 1, 0), oma = c(0,0,0,0))
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, col = caterpillarCol,add = FALSE, ylim = c(0, .6), 
                main = "NC Botanical Garden", plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'beetle', col = beetleCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'spider', col = spiderCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'leafhopper', col = hopperCol,add = TRUE, plotVar = 'meanDensity')
annualTrendPlot(fullDataset, "NC Botanical Garden", lwd = 5, group = 'truebugs', col = truebugCol,add = TRUE, plotVar = 'meanDensity')
legend("topright", legend = c("beetles", "spiders", "leafhoppers", "caterpillars", "true bugs"),
       col = c(beetleCol, spiderCol, hopperCol, caterpillarCol, truebugCol), lwd = 5, cex = .8)
dev.off()
