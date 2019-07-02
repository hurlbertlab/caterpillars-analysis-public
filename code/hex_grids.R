### Function for grouping lat-lon point observations into hex cells

library(tidyverse)
library(sf)

#### Read in data, make hex grid

hex <- dggridR::dgconstruct(res = 6)

NAmap <- read_sf('data/maps/ne_50m_admin_1_states_provinces_lakes.shp') %>%
  st_crop(c(ymin = 25, xmin = -105, ymax = 46, xmax = -60)) 

cats <- read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE) %>%
  filter(!is.na(longitude), !is.na(latitude))

cats$cell <- dgGEO_to_SEQNUM(hex, cats$longitude, cats$latitude)$seqnum + 0.1


#### Convert hex grid to sf format for plotting/data joining

grid <- dgearthgrid(hex, frame = TRUE)
grid_list <- split(grid[,c("long","lat")], grid$group)
ps <- sapply(grid_list, Polygon)
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = names(grid_list)[i]))
grid_list_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84"))
grid_list_polys_df <- SpatialPolygonsDataFrame(grid_list_polys, data.frame(id = names(grid_list), row.names = names(grid_list)))

hex_sf <- st_as_sf(grid_list_polys_df) %>%
  st_crop(st_bbox(NAmap)) 

cats_sf <- cats %>%
  group_by(cell) %>%
  summarize(n = n()) %>%
  mutate(cell = factor(cell))

hex_cats <- hex_sf %>%
  right_join(cats_sf, by = c("id" = "cell"))

library(tmap)
tm_shape(NAmap) + tm_polygons() + tm_shape(hex_cats) + tm_polygons(col = "n",, alpha = 0.7)


