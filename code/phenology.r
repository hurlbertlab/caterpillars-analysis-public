# Script for displaying caterpillar phenology across sites

library(dplyr)
library(lubridate)
library(sp)
library(maps)
library(maptools)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')


# Criteria for inclusion (records refers to survey events)
siteList = function(fullDataset, year, minNumRecords = 40, minNumDates = 4, write = TRUE) {
  out = fullDataset %>%
    filter(Year == year) %>%
    group_by(Name, Region, Latitude, Longitude) %>%
    summarize(nSurvs = n_distinct(ID),
              nDates = n_distinct(LocalDate),
              nCat = sum(Group == 'caterpillar', na.rm = TRUE),
              pctCat = round(nCat/nSurvs, 3),
              nArth = sum(Quantity, na.rm = TRUE),
              nLgArth = sum(Quantity[Length >= 10], na.rm = TRUE),
              nArthsPerSurvey = nArth/nSurvs,
              nLgArthsPerSurvey = nLgArth/nSurvs,
              pctSurvsLgArths = round(sum(Length >= 10, na.rm = TRUE)/nSurvs, 3),
              nPhoto = sum(Photo, na.rm = TRUE),
              pctPhoto = round(nPhoto/nArth, 3)) %>%
    arrange(desc(Latitude)) %>%
    filter(nSurvs >= minNumRecords, nDates >= minNumDates, Name != "Example Site") %>%
    mutate(county = latlong2county(data.frame(lon = Longitude, lat = Latitude)))
  
  if (write) {
    write.table(out, paste('data/sitelist', year, '.txt', sep = ''), sep = '\t', row.names = F)
  }
  return(out)
}

# If a sitename string is too long, find the best space position for breaking into
# two separate lines. If it is not too long return NA.
breakPosition = function(string, maxCharsPerLine = 25) {
  
  if (nchar(string) <= maxCharsPerLine) {
    lineBreak = NA
  } else {
    breaks = gregexpr(" ", string)
    lineBreak = min(breaks[[1]][breaks[[1]] >= nchar(string)/2])
  }
  return(lineBreak)
}

siteNameForPlotting = function(sitename, maxCharsPerLine = 25) {
  breakPos = breakPosition(sitename, maxCharsPerLine)
  
  newname = ifelse(is.na(breakPos), sitename, 
                   paste(substr(sitename, 1, breakPos - 1), "\n", 
                         substr(sitename, breakPos + 1, nchar(sitename)), sep = ""))
  return(newname)
}


multiSitePhenoPlot = function(fullDataset, 
                              year, 
                              siteList, 
                              write = TRUE, 
                              monthRange = NULL, # 2-value vector with beginning and ending months for plotting;
                                                 # e.g., start of May - end of August would be c(5,8).
                                                 # If NULL, xlim will vary by site based on when surveys were conducted
                              REVI = FALSE,      # plot window of red-eyed vireo nestlings estimated from eBird
                                                 # (requires manual addition of REVI columns to siteList)
                              filename,
                              panelRows = 4,
                              panelCols = 6,
                              colRGB = c(0, .5, 0), #vector of R, G, and B color values
                              cex.main = 1.5,
                              cex.lab = 1,
                              cex.axis = 1,
                              cex.text = 1.5,
                              ...) {

  if (write) {
    pdf(paste('figs/', filename, '.pdf', sep = ''), height = 8.5, width = 11)
  }
  
  # Concatenate region name to the end of site name (if it's not already there)
  siteList$siteNameRegion = apply(siteList, 1, function(x) 
    ifelse(substr(x[1], nchar(x[1])-3, nchar(x[1])) == paste(", ", x[2], sep = ""),
           x[1], paste(x[1], ", ", x[2], sep = "")))
  
  
  
  par(mfrow = c(panelRows, panelCols), mar = c(3, 2, 3, 1), oma = c(5, 5, 0, 0))
  
  counter = 0
  
  for (site in siteList$Name) {
    
    counter = counter + 1
    sitedata = fullDataset %>%
      filter(Name == site, Year == year)
    
    siteLabel = siteNameForPlotting(siteList$siteNameRegion[siteList$Name == site], maxCharsPerLine = 23)
    
    # goofy temporary correction for long name
    siteLabel[siteLabel == "Litzsinger Road Ecology Center\nWoodland Site A, MO"] = "Litzsinger Road Ecology\nCenter Site A, MO"
    
    
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
    
    # Caterpillar phenology
    caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                            plot = FALSE, plotVar = 'fracSurveys', allDates = FALSE, ...)
    
    
    caterpillarPhenology = meanDensityByDay(sitedata, ordersToInclude = 'caterpillar', 
                                            plot = TRUE, plotVar = 'fracSurveys', allDates = FALSE, xlab = 'Date',
                                            ylab = 'Percent of surveys', lwd = 3, 
                                            xaxt = 'n', xaxs = 'i', cex.lab = cex.lab, cex.axis = cex.axis,
                                            xlim = c(jds[minPos], jds[maxPos]),
                                            ylim = c(0, max(1, 1.3*max(caterpillarPhenology$fracSurveys))), 
                                            main = siteLabel, cex.main = cex.main,
                                            col = rgb(colRGB[1], colRGB[2], colRGB[3]), ...)
    
    text(jds[minPos] + 5, 1.2*max(caterpillarPhenology$fracSurveys), paste(siteList$nSurvs[siteList$Name == site], "surveys"),
         col = 'blue', cex = cex.text, adj = 0)
    text(jds[maxPos] - 2, 1.2*max(caterpillarPhenology$fracSurveys), paste(round(siteList$Latitude[siteList$Name == site], 1), "°N", sep = ""),
         col = 'red', cex = cex.text, adj = 1)
    
    abline(v = jds, col = 'gray50')
    mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = cex.axis, line = .25)
    
    if (REVI) {
      bird = siteList %>%
        filter(Name == site) %>%
        mutate(preArrival = yday(as.Date(LatestWeekWithFreq0, format = "%m/%d/%Y")) + 3, # +3 to shift from beg to middle of week
             peakArrival = yday(as.Date(WeekOfPeakFreq, format = "%m/%d/%Y")) + 3,
             arrival = round((preArrival + peakArrival)/2),
             hatching = arrival + 35,
             fledging = hatching + 11)
      rect(bird$hatching, -5, bird$fledging, 110, col = rgb(colRGB[1], colRGB[2], colRGB[3], .1), border = NA)
    }
    
    #if (counter %% panelRows*panelCols == 0 | counter == nrow(siteList)) {
    #  mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
    #  mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 1, cex = 1.5)
    #}  
  } #end site

  mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
  mtext("Percent of surveys with caterpillars", 2, outer = TRUE, line = 1, cex = 1.5)

  if (write) {
    dev.off()
  }
}  
  





############################################################
# Interpolate phenology values on a daily basis for the purpose
# of color coding line segements over time

interpolatePhenoByDay = function(phenodata, var = 'fracSurveys') {
  # phenodata is object created by meanDensityByDay()
  # var can be either 'fracSurveys' or 'meanDensity'
  
  days = data.frame(julianday = min(phenodata$julianday):max(phenodata$julianday))
  
  phenodat = phenodata[, c('julianday', var)]
  names(phenodat)[2] = 'x'
  
  pheno = days %>% 
    left_join(phenodat, by = 'julianday')
  
  # Find interior NAs
  intNAs = which(sapply(1:nrow(pheno), function(row) is.na(pheno$x[row]) &
                          sum(pheno$x[1:(row-1)], na.rm = TRUE) >= 0 &
                          sum(pheno$x[(row+1):nrow(pheno)], na.rm = TRUE) >= 0))
  
  if (length(intNAs) > 0) {
    for (i in intNAs) {
      preValPos = max(which(!is.na(pheno$x[1:(i-1)])))
      postValPos = min(which(!is.na(pheno$x[(i+1):nrow(pheno)]))) + i
      
      slope = (pheno$x[postValPos] - pheno$x[preValPos])/(pheno$julianday[postValPos] - pheno$julianday[preValPos])
      
      pheno$x[i] = pheno$x[preValPos] + slope*(pheno$julianday[i] - pheno$julianday[preValPos])
    }
  }
  return(pheno)
}




# Take an interpolated pheno object as returned by interpolatePheno()
# and plot phenocurve with line rainbow-colored by date
rainbowPhenoPlot = function(phenodata, minJD = 95, maxJD = 221, ...) {

  colors = c('#2F2C62', '#42399B', '#4A52A7', '#59AFEA', '#7BCEB8', '#A7DA64',
             '#EFF121', '#F5952D', '#E93131', '#D70131')
  col.ramp = colorRampPalette(colors)
  cols = data.frame(julianday = minJD:maxJD, 
                    col = col.ramp(length(minJD:maxJD)))
  
  phenocol = cols %>%
    left_join(phenodata, by = 'julianday')
  phenocol$col = as.character(phenocol$col)
    
  x = phenocol$julianday
  y = phenocol$x

  par(bg = NA)
  plot(x, y, xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = 'n', bty = 'n')
  
  # Plot the colored line segments  
  sapply(1:(nrow(phenocol) - 1), function(jd) 
    segments(x0 = x[jd], y0 = y[jd], x1 = x[jd + 1], y1 = y[jd + 1], col = phenocol$col[jd], ...))
  
  # Plot month bar along the bottom
  
}
  




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

rainbowScaleBar = function(minJD = 91, maxJD = 228, plot = TRUE) {
  colors = c('#2F2C62', '#42399B', '#4A52A7', '#59AFEA', '#7BCEB8', '#A7DA64',
             '#EFF121', '#F5952D', '#E93131', '#D70131')
  col.ramp = colorRampPalette(colors)
  cols = data.frame(julianday = minJD:maxJD, 
                    col = col.ramp(length(minJD:maxJD)))

  # labels
  monthLabels = data.frame(jd = c(1, 15, 32, 46, 60, 74, 91, 105, 121, 135, 152, 166, 
                                  182, 196, 213, 227, 244, 258, 274, 288, 305, 319, 335, 349),
                           
                           date = c("Jan 1", "Jan 15", "Feb 1", "Feb 15", "Mar 1", 
                                    "Mar 15", "Apr 1", "Apr 15", "May 1", "May 15", 
                                    "Jun 1", "Jun 15", "Jul 1", "Jul 15", "Aug 1", 
                                    "Aug 15", "Sep 1", "Sep 15", "Oct 1", "Oct 15", 
                                    "Nov 1", "Nov 15", "Dec 1", "Dec 15"))
  
  bar = left_join(cols, monthLabels, by = c('julianday' = 'jd'))
  bar$col = as.character(bar$col)
  
  barlabs = bar[!is.na(bar$date), ]
  
  if (plot) {
    png('figs/rainbow_scale.png', height = 600, width = 150, bg = NA)
    par(mar = c(0,0,0,0))
    plot(rep(1, nrow(bar)), -bar$julianday, pch = 15, cex = 4, col = bar$col,
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n', xlim = c(.9, 3.5))
    text(rep(1.4, nrow(barlabs)), -barlabs$julianday, barlabs$date, adj = 0, cex = 3)
    dev.off()
  }
}




















# Top 12 sites without ECU, weighted mean fraction of surveys with caterpillars
## GD

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