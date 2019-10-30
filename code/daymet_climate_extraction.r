# Use daymetr to get daily temperature data at lat-longs (up through year before present)
library(daymetr)

climatedata = data.frame(year = NA, yday = NA, dayl..s. = NA, prcp..mm.day. = NA, srad..W.m.2. = NA,
                         swe..kg.m.2. = NA, tmax..deg.c. = NA, tmin..deg.c. = NA, vp..Pa. = NA)
for (s in unique(fullDataset$Name)) {
  minYear = min(fullDataset$Year[fullDataset$Name == s], na.rm = T)
  maxYear = max(fullDataset$Year[fullDataset$Name == s], na.rm = T)
  
  # Data are unavailable for current year, so can only download up through "last year"
  if (maxYear == as.numeric(format(Sys.Date(), "%Y"))) {
    maxYear = maxYear - 1
  }
  
  tmp = tryCatch({download_daymet(lat = sites$Latitude[sites$Name == s], 
                                  lon = sites$Longitude[sites$Name ==s], 
                                  start = minYear, end = maxYear, internal = TRUE)}, 
                 error = function(e) { tmp = NULL })
  
  if (!is.null(tmp)) {
    climatedata = rbind(climatedata, tmp$data)
  }
}

write.csv(climatedata, 'data/env/daymet_climate.csv', row.names = F)
