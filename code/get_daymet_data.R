## Climate data for eastern North America

library(daymetr)
library(tidyverse)
library(raster)
library(ncdf4)
library(lubridate)
library(sf)

setwd("\\\\BioArk\\HurlbertLab\\GIS\\")

# Daymet: eastern North America, monthly average temperatures for March-May

download_daymet_ncss(location = c(49, -101, 24, -69),
                      start = 2015,
                      end = 2018,
                      param = "tmin", 
                      path = "daymet/")

nc2tif(path = "daymet/")

# Read in data for pre-2018
daymet_raster <- stack("daymet/tmin_monavg_2017_ncss.tif")

daymet_spring <- daymet_raster[[3:5]]
daymet_points <- rasterToPoints(daymet_spring)

# Need to convert 2018 to monthly averages

nc <- nc_open("daymet/daymet_v3_tmin_2018_na.nc4")
na18 <- brick("daymet/daymet_v3_tmin_2018_na.nc4")

# Check if any -999 values - change to NAs

monthly_mean <- function(dailyraster, mon) {
  raster <- dailyraster
  dates <- data.frame(layer = names(na18)) %>%
    mutate(format = substr(layer, 2, 11), 
           date = as.Date(format, format = "%Y.%m.%d"),
           month = month(date)) %>%
    filter(month == mon)
  mon_raster <- raster[[dates$layer]]
  calc(mon_raster, mean)
}

march <- monthly_mean(na18, 3)
april <- monthly_mean(na18, 4)
may <- monthly_mean(na18, 5)

spring18 <- stack(march, april, may)
eNA_mu <- extent(c(-1000000, 3000000, -2000000, 2000000))
spring18_eNA <- crop(spring18, eNA_mu)
names(spring18_eNA) <- c("march2018", "april2018", "may2018")
writeRaster(spring18_eNA, "daymet/tmin_spring_2018")

spring18_eNA <- stack("daymet/tmin_spring_2018")

spring18_eNA_coarse <- aggregate(spring18_eNA, fact = 300)

# This all doesn't work
spring18_transf <- projectRaster(spring18_eNA_coarse, daymet_spring)
spring18_points <- data.frame(rasterToPoints(spring18_eNA_coarse))
spring18_sf <- st_as_sf(spring18_points, coords = c("x", "y"), 
                        crs = "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +a=60 +rf=6378137 +lat_2=45") %>%
  st_transform(crs = 4269)
