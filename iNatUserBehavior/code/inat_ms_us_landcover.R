### iNaturalist through 2019 land cover

library(dplyr)
library(raster)
library(sf)

## On HPC Longleaf: extract land cover at iNat sites

na_lc <- raster("/proj/hurlbertlab/nlcd_landcover/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
na_crs <- crs(na_lc)

inat_northam_sites <- read.csv("/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_northam_site_coords.csv")

inat_sites <- inat_northam_sites %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs)

inat_lc <- extract(na_lc, inat_sites)

inat_sites$landcover <- inat_lc

inat_sites_df <- inat_sites %>%
  st_set_geometry(NULL)
write.csv(inat_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_us_site_landcover.csv", row.names = F)
