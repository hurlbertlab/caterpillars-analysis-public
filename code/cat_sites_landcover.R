### Land cover of caterpillars count and iNaturalist caterpillar observations

library(tidyverse)
library(raster)
library(sf)

# plotting theme
theme_set(theme_classic(base_size = 15))

# Source caterpillars count data
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# Read in iNat caterpillars of eastern North America project data
inat <- read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

# CRS from CEC Land cover map for North America, 2015
na_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0"

catcount_sites <- fullDataset %>%
  distinct(Latitude, Longitude) %>%
  rename(latitude = "Latitude", longitude = "Longitude") %>%
  mutate(dataset = "CC")

inat_sites <- inat %>%
  distinct(latitude, longitude) %>%
  mutate(dataset = "iNat")

cat_sites <- bind_rows(catcount_sites, inat_sites) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs)
# st_write(cat_sites, "data/inat_catcount_site_coords.shp", delete_dsn=TRUE)

## On HPC Longleaf: extract land cover at sites from iNaturalist caterpillars and caterpillars count

# na_lc <- raster("/proj/hurlbertlab/cec_north_america/north_america_2015/NA_NALCMS_2015_LC_30m_LAEA_mmu5pix_.tif")
# cat_sites <- read_sf("/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_catcount_site_coords.shp")
# 
# cat_lc <- extract(na_lc, cat_sites)
# 
# cat_sites$landcover <- cat_lc
# 
# cat_sites_df <- cat_sites %>%
#   st_set_geometry(NULL)
# write.csv(cat_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_catcount_site_landcover.csv", row.names = F)

## Figure: comparison of land cover distributions for iNat and CC records

cat_sites_df <- read.csv("data/inat_catcount_site_landcover.csv")
cec_legend <- read.table("data/cec_landcover_legend.txt", header = T, sep = ",", stringsAsFactors = F)

cat_sites_lc <- cat_sites_df %>%
  left_join(cec_legend, by = c("landcover" = "value"))

cc_sites <- fullDataset %>%
  distinct(Latitude, Longitude, Name, Region)

cc_sites_lc <- cat_sites_lc %>%
  filter(dataset == "CC") %>%
  left_join(cc_sites, by = c("lat" = "Latitude", "lon" = "Longitude"))

cat_sites_dist <- cat_sites_lc %>%
  group_by(dataset, legend) %>%
  summarize(n_recs = n()) %>%
  group_by(dataset) %>%
  mutate(total_recs = sum(n_recs),
         prop_recs = n_recs/total_recs) %>%
  filter(!is.na(legend))

ggplot(cat_sites_dist, aes(x = dataset, y = prop_recs, fill = legend)) + 
  geom_col(position = "stack") + coord_flip() +
  labs(x = "", y = "Proportion of sites", fill = "") + scale_fill_viridis_d()
ggsave("figs/inaturalist/landcover_types_inat_cc.pdf", units = "in", width = 10, height = 5)

