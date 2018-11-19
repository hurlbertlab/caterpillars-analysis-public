# Examining caterpillar phenology with iNaturalist data

# iNaturalist data requested in October 2018

library(dplyr)
library(taxize)
library(lubridate)
library(maps)
library(rgdal)


# Read in data
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

NAmap = readOGR('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

# Aggregate iNat data by day, year, and lat-lon bin
inat$observed_on = as.Date(inat$observed_on, format = "%Y-%m-%d")
inat$year = format(inat$observed_on, format = "%Y")
inat$jday = yday(inat$observed_on)
inat$jd_wk = 7*floor(inat$jday/7) + 4    # week 1 = jd 4, week 2 = jd 11, etc

inat$lat_round = round(inat$latitude, 1)
inat$lon_round = round(inat$longitude, 1)


# Functions

# Count number of records per year and lat-long bin as a function of bin size (in degrees)
recsByBinYear = function(binsize) {
  inat$lat_bin = binsize*floor(inat$latitude/binsize) + binsize/2
  inat$lon_bin = binsize*floor(inat$longitude/binsize) + binsize/2

  inat_by_latlon_yr = inat %>%
    group_by(lat_bin, lon_bin, year) %>%
    count()
  
  return(inat_by_latlon_yr)
}


# For a given lat-lon cell and year, plot a line graph of phenology color-coded
# (blue segments correspond to early in the year, red segments to late in the year)
phenoMapPlot = function(yr, latCenter, lonCenter, binWidth = 2, plotScaleX = 2, plotScaleY = 2, ...) {

  # Colors
  colors = c('#2F2C62', '#42399B', '#4A52A7', '#59AFEA', '#7BCEB8', '#A7DA64',
             '#EFF121', '#F5952D', '#E93131', '#D70131', '#D70131')
  col.ramp = colorRampPalette(colors)
  cols = data.frame(jd_wk = seq(4, 362, 7), col = col.ramp(52))
  
  pheno = inat %>%
    filter(year == yr, 
           latitude >= latCenter - 0.5*binWidth, latitude < latCenter + 0.5*binWidth,
           longitude >= lonCenter - 0.5*binWidth, longitude < lonCenter + 0.5*binWidth) %>%
    select(jday, jd_wk, latitude, longitude, scientific_name) %>%
    distinct() %>%
    group_by(jd_wk) %>%
    count() %>%
    left_join(cols, by = 'jd_wk') 
  pheno$col = as.character(pheno$col)
  
  x = lonCenter - 0.5*plotScaleX + plotScaleX*pheno$jd_wk/365
  y = latCenter - 0.5*plotScaleY + plotScaleY*pheno$n/max(pheno$n)
  
  sapply(1:(nrow(pheno) - 1), function(jd) 
    segments(x0 = x[jd], y0 = y[jd], x1 = x[jd + 1], y1 = y[jd + 1], col = pheno$col[jd], ...))
  
}


binsize = 2 #degrees

recsPerBinThreshold = 100
pdf(paste('figs/iNat_caterpillar_phenomap_byYear_', binsize, 'deg_bin.pdf', sep = ''), height = 8, width = 10)
for (y in 2013:2018) {
  inatBins = recsByBinYear(binsize) %>%
    filter(year == y, n >= recsPerBinThreshold)
  
  plot(NAmap, xlim = c(-100, -64), ylim = c(25, 50), border = 'gray80', col = 'gray90')
  text(-85, 50, y, cex = 2)
  mtext(paste("iNaturalist caterpillar observations, >=", recsPerBinThreshold, "records per bin"), 1, cex = 1.5)
  for (i in 1:nrow(inatBins)) {
    phenoMapPlot(yr = y, lat = inatBins$lat_bin[i], lon = inatBins$lon_bin[i], 
                 lwd = 2, binWidth = binsize, plotScaleY = .7*binsize, plotScaleX = .85*binsize)
  }
  points(rep(-70, 20), seq(28, 38, length.out = 20), pch = 15, cex = 2, col = col.ramp(20))
  text(rep(-68, 5), seq(28, 38, length.out = 5), c('4 Jan', '3 Apr', '1 Jul', '29 Sep', '27 Dec'))
}
dev.off()
