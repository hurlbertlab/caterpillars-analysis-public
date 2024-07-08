# Map of potential MM2.0 sites

library(ggplot2)
library(dplyr)
library(gsheet)
library(sf)

url = "https://docs.google.com/spreadsheets/d/1ncBl8OQFeTrTh6-CgeKSJ9GlcZeVBBeXonvA0Ji-hGM/edit#gid=0"

sites = gsheet2tbl(url)

NAmap <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes') %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN"))


cc_sites <- NAmap %>%
  mutate(cc = as.factor(ifelse(postal %in% states, 1, 0))) %>%
  st_transform(crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")

# Set colors for states with/without CC site
cols <- c("gray95", rgb(93/255, 156/255, 47/255))


# Creating and then projecting site coordinates
sites2 = sites %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = "+proj=longlat")
sites3 = st_transform(sites2, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35") %>%
  mutate(color = ifelse(existing == "Y", "limegreen", "gray20"))

# Plot points on map, in this case within bounding box of eastern NA
cc_map <- tm_shape(cc_sites, bbox=tmaptools::bb(matrix(c(-100000, -1007822, 3000000, 2000000),2,2))) + 
  tm_polygons(col = "gray95", palette = cols, legend.show = F) +
  tm_shape(sites3) +
  tm_symbols(size = .75, col = "color") +
  tm_text("ID", xmod = .7, size = .9)

# Save map as PDF
tmap_save(cc_map, "figs/potential_MM2.0_sites_2024.png")

