source('code/analysis_functions.r')

fullDataset = read.csv('data/fullDataset_2025-11-10.csv', header = T)

# Getting 2025 sites with 100+ surveys over 7+ weeks (excl UGA which doesn't survey in summer)
sitesum2025 = siteSummary(fullDataset, 2025, minNumRecords = 100, minNumWeeks = 7, write = F) |> 
  filter(Name != "East Campus UGA")

# Subset of sites with same minimum effort also in 2024
sitesum2024 = siteSummary(fullDataset, 2024, minNumRecords = 100, minNumWeeks = 7, write = F) |> 
  filter(Name %in% sitesum2025$Name) %>%
  mutate(siteNameRegion = paste0(Name, ", ", Region))

monthRange = c(5, 8)
plotVar = 'fracSurveys'
cex.lab = 1
cex.axis = .8
cex.main = 1
cex.text = 1
panelRows = 4
panelCols = 6

par(mfrow = c(4, 6), mar = c(3, 2, 3, 1), oma = c(5, 5, 0, 0))

counter = 0

for (site in sitesum2024$Name) {

  counter = counter + 1
  
  # x-axis labels
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
  
  siteLabel = siteNameForPlotting(sitesum2024$siteNameRegion[sitesum2024$Name == site], maxCharsPerLine = 23)
  
  # goofy temporary correction for long name
  if (site == "Triangle Land Conservancy - Johnston Mill Nature Preserve") { siteLabel = "Johnston Mill, NC" }
  
  sitedata24 = fullDataset %>%
    filter(Name == site, Year == 2024)
  
  sitedata25 = fullDataset %>%
    filter(Name == site, Year == 2025)

    # Caterpillar phenology
    caterpillarPhenology24 = meanDensityByWeek(sitedata24, plotVar = plotVar,
                                               ordersToInclude = 'caterpillar',
                                             plot = FALSE, allDates = FALSE)
    
    caterpillarPhenology25 = meanDensityByWeek(sitedata25, plotVar = plotVar,
                                               ordersToInclude = 'caterpillar',
                                               plot = FALSE, allDates = FALSE)
    

    if (plotVar == 'fracSurveys') {
      yLabel = 'Percent of surveys'
      minY = 0
    } else if (plotVar == 'meanDensity') {
      yLabel = 'Density (# / survey)' 
      minY = min(caterpillarPhenology[, plotVar], na.rm = TRUE)
    } else if (plotVar == 'meanBiomass') {
      yLabel = 'Biomass (mg / survey)'
      #minY = min(caterpillarPhenology[, plotVar], na.rm = TRUE)
      minY = 0
    }
    maxY = max(1.3*max(c(caterpillarPhenology24[, plotVar], 
                         caterpillarPhenology25[, plotVar])), 1)
    
    # Set up plot frame
    caterpillarPhenology24 = meanDensityByWeek(sitedata24, plotVar = plotVar,
                                             plot = TRUE, new = TRUE, 
                                             allDates = FALSE, xlab = 'Date',
                                             ylab = yLabel, lwd = 3, 
                                             xaxt = 'n', xaxs = 'i', cex.lab = cex.lab, 
                                             cex.axis = cex.axis,
                                             color = 'palegreen3',
                                             xlim = c(jds[minPos], jds[maxPos]),
                                             ylim = c(minY, maxY), 
                                             main = siteLabel, cex.main = cex.main,
                                             allCats = TRUE, 
                                             ordersToInclude = 'caterpillar')


    caterpillarPhenology25 = meanDensityByWeek(sitedata25, plotVar = plotVar,
                                             plot = TRUE, new = FALSE, 
                                             allDates = FALSE, xlab = 'Date',
                                             ylab = yLabel, lwd = 3, 
                                             xaxt = 'n', xaxs = 'i', cex.lab = cex.lab, 
                                             cex.axis = cex.axis,
                                             color = 'purple',
                                             xlim = c(jds[minPos], jds[maxPos]),
                                             ylim = c(minY, maxY), 
                                             main = siteLabel, cex.main = cex.main,
                                             allCats = TRUE, 
                                             ordersToInclude = 'caterpillar')
    
    text(jds[minPos] + 5, .9*maxY, 
         sitesum2024$nSurveys[sitesum2024$Name == site] +
           sitesum2025$nSurveys[sitesum2025$Name == site],
         col = 'blue', cex = cex.text, adj = 0)
    text(jds[maxPos] - 2, .9*maxY, paste(round(sitesum2024$Latitude[sitesum2024$Name == site], 1), "°N", sep = ""),
         col = 'red', cex = cex.text, adj = 1)
    
    mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = cex.axis, line = .25)
    
    
    if (counter %% (panelRows*panelCols) == 0 | counter == nrow(sitesum2024)) {
      mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
      mtext(yLabel, 2, outer = TRUE, line = 1, cex = 1.5)
    }  
    
} # end site loop 

plot(1, type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', bty = 'n')
legend('topleft', legend = c(2024, 2025), col = c('palegreen3', 'purple'), 
       lty = 'solid', lwd = 5, cex = 1.8, bty = 'n')
