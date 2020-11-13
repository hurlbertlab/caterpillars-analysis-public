### iNaturalist through 2019 land cover

library(tidyverse)
library(raster)
library(sf)

## On HPC Longleaf: extract land cover at iNat sites

## Landcover CRS
na_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

inat_northam_sites <- read.csv("/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_northam_site_coords.csv")

inat_sites <- inat_northam_sites %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs)

na_lc <- raster("/proj/hurlbertlab/cec_north_america/north_america_2015/NA_NALCMS_2015_LC_30m_LAEA_mmu5pix_.tif")

inat_lc <- extract(na_lc, inat_sites)

inat_sites$landcover <- inat_lc

inat_sites_df <- inat_sites %>%
  st_set_geometry(NULL)
write.csv(inat_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_northam_site_landcover.csv", row.names = F)
