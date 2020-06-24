### Land cover of caterpillars count and iNaturalist caterpillar observations

library(tidyverse)
library(raster)
library(sf)
library(tmap)

# plotting theme
theme_set(theme_classic(base_size = 15))

# Map of north america

na_map <- read_sf("data/maps/ne_50m_admin_1_states_provinces_lakes.shp") %>%
  filter(sr_adm0_a3 == "CAN" | sr_adm0_a3 == "USA")

# Source caterpillars count data
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# Read in iNat caterpillars of eastern North America project data
inat <- read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

# CRS from CEC Land cover map for North America, 2015
na_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

catcount_sites <- fullDataset %>%
  filter(Region != "CA", Region != "ON", !(grepl("BBS", Name))) %>%
  distinct(Name, Latitude, Longitude) %>%
  rename(latitude = "Latitude", longitude = "Longitude") %>%
  mutate(dataset = "CC") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs) %>%
  st_buffer(75)
# st_write(catcount_sites, "data/catcount_site_buffers.shp")

inat_sites <- inat %>%
  distinct(latitude, longitude) %>%
  mutate(dataset = "iNat") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(na_crs)
# st_write(inat_sites, "data/inat_cats_site_coords.shp")

## On HPC Longleaf: extract land cover at sites from iNaturalist caterpillars and caterpillars count

# na_lc <- raster("/proj/hurlbertlab/nlcd_landcover/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
# catcount_sites <- read_sf("/proj/hurlbertlab/gdicecco/caterpillars-analysis/catcount_site_buffers.shp")
# inat_sites <- read_sf("/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_cats_site_coords.shp")
# 
# # Extract LC at points for iNat data
# inat_lc <- extract(na_lc, inat_sites)
# 
# inat_sites$landcover <- inat_lc
# 
# cat_sites_df <- inat_sites %>%
#   st_set_geometry(NULL)
# write.csv(cat_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_site_landcover.csv", row.names = F)
# 
# # Modal land cover at buffers for CatCount sites
# 
# # mode
# getmode <- function(v) {
#     uniqv <- unique(v)
#     uniqv[which.max(tabulate(match(v, uniqv)))]
#  }
# 
# catcount_sites$landcover <- NA
# for(site in catcount_sites$Name) {
#   cc_site <- catcount_sites %>%
#     filter(Name == site)
#   
#   lc_crop <- crop(na_lc, cc_site)
#   lc_mask <- mask(lc_crop, cc_site)
#   
#   vals <- na.omit(values(lc_mask))
#   mode <- getmode(vals)
#   
#   catcount_sites$landcover[catcount_sites$Name == site] <- mode
# }
# 
# catcount_sites_df <- catcount_sites %>%
#   st_set_geometry(NULL)
# write.csv(catcount_sites_df, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/catcount_site_landcover.csv", row.names = F)

## Figure: comparison of land cover distributions for iNat and CC records

catcount_sites_df <- read.csv("data/catcount_site_landcover.csv", stringsAsFactors = F)
inat_sites_df <- read.csv("data/inat_site_landcover.csv", stringsAsFactors = F)
nlcd_legend <- read.csv("data/nlcd2016_legend.csv", stringsAsFactors = F)

nlcd_order <- c("Open Water", "Developed Open Space", "Developed Low Intensity", "Developed Medium Intensity", 
                "Developed High Intensity", "Barren Land (Rocky/Sand/Clay)",
                "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
                "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay",
                "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")

cat_sites_lc <- inat_sites_df %>%
  mutate(Name = NA) %>%
  bind_rows(catcount_sites_df) %>%
  left_join(nlcd_legend, by = c("landcover" = "class"))

cc_sites_lc <- cat_sites_lc %>%
  filter(dataset == "CC")

cat_sites_dist <- cat_sites_lc %>%
  filter(!is.na(legend)) %>%
  group_by(dataset, legend) %>%
  summarize(n_recs = n()) %>%
  group_by(dataset) %>%
  mutate(total_recs = sum(n_recs),
         prop_recs = n_recs/total_recs) 

nlcd_palette <- c("Open Water" = "#788cbe", "Developed Open Space" = "#dec9c9", "Developed Low Intensity" = "#fde5e4",
             "Developed Medium Intensity" = "#f7b29f", "Developed High Intensity" = "#e7564e",
             "Barren Land (Rocky/Sand/Clay)" = "#b3ada3",
             "Deciduous Forest" = "#69ab63", "Evergreen Forest" = "#1c6330", "Mixed Forest" = "#b5c98f",
             "Shrub/Scrub" = "#ccba7d", "Grassland/Herbaceous" = "#e3e3c2", "Pasture/Hay" = "#dbd93d",
             "Cultivated Crops" = "#ab7029", "Woody Wetlands" = "#bad9eb", "Emergent Herbaceous Wetlands" = "#70a3ba")

ggplot(cat_sites_dist, aes(x = dataset, y = prop_recs, fill = fct_relevel(legend, nlcd_order))) +
  geom_col(position = "stack") + coord_flip() +
  labs(x = "", y = "Proportion of sites", fill = "") + scale_fill_manual(values = nlcd_palette)
# ggsave("figs/cross-comparisons/landcover_types_inat_cc.pdf", units = "in", width = 10, height = 5)

## Map of CC land cover

cc_sites_sf <- cc_sites_lc %>%
  st_as_sf(coords = c("lon", "lat"))

us_map <- na_map %>%
  st_crop(xmin = -100, xmax = -53, ymin = 20, ymax = 60)

cc_site_lc_map <- tm_shape(us_map) + tm_polygons() + tm_shape(cc_sites_sf) +
  tm_dots(size = 0.5, col = "legend", palette = "Paired", title = "Land cover") +
  tm_layout(legend.text.size = 0.75, legend.title.size = 1)
# tmap_save(cc_site_lc_map, "figs/caterpillars-count/cc_site_landcover_map.pdf", units = "in", height = 6, width= 8)
