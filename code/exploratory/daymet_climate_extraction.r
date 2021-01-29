# Use daymetr to get daily temperature data at lat-longs (up through year before present)
library(daymetr)

climatedata = data.frame(Name = NA, longitude = NA, latitude = NA, year = NA, yday = NA, prcp..mm.day. = NA, 
                         tmax..deg.c. = NA, tmin..deg.c. = NA)
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
    tmp$data$longitude = sites$Longitude[sites$Name == s]
    tmp$data$latitude = sites$Latitude[sites$Name == s]
    tmp$data$Name = s
    tmp$data = tmp$data[, names(climatedata)]
        climatedata = rbind(climatedata, tmp$data)
  }
}

climatedata = climatedata[-1, ]
climatedata$date = as.Date(climatedata$yday, origin = paste0(climatedata$year-1, "-12-31"))
write.csv(climatedata, 'data/env/daymet_climate.csv', row.names = F)



