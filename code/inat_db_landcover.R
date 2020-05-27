### iNaturalist database land cover

library(tidyverse)
library(raster)
library(sf)

# ## CRS of land cover
# na_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
# +ellps=WGS84 +towgs84=0,0,0"
# 
# ## Get coords from iNaturalist database
# 
# library(dbplyr)
# 
# setwd("\\\\BioArk/HurlbertLab/Databases/iNaturalist/inat_thru_2019")
# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inat_2019.db")
# 
# db_list_tables(con)
# 
# inat_sites_db <- tbl(con, "inat") %>%
#   filter(!is.na(longitude) | !is.na(latitude)) %>%
#   filter(latitude > 15, latitude < 90, longitude > -180, longitude < -30) %>%
#   dplyr::select(longitude, latitude) %>%
#   distinct()
# 
# inat_sites_df <- inat_sites_db %>%
#   collect()
# 
# inat_sites_sf <- inat_sites_df %>%
#   mutate(lat = latitude,
#          lon = longitude) %>%
#   filter(!is.na(latitude), !is.na(longitude)) %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_set_crs(4326) %>%
#   st_transform(na_crs)
# st_write(inat_sites_sf, "data/inat_northam_site_coords.shp")

## On HPC Longleaf: extract land cover at sites from iNaturalist caterpillars and caterpillars count

na_lc <- raster("/proj/hurlbertlab/cec_north_america/north_america_2015/NA_NALCMS_2015_LC_30m_LAEA_mmu5pix_.tif")
inat_sites <- read_sf("/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_northam_site_coords.shp")

inat_lc <- extract(na_lc, inat_sites)

inat_sites$landcover <- inat_lc

inat_sites_df <- inat_sites %>%
  st_set_geometry(NULL)
write.csv(inat_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_northam_site_landcover.csv", row.names = F)
