# Code for plotting a map of the states and provinces with CC sites

library(sf)
library(tidyverse)
library(tmap)

source('code/reading_datafiles_without_users.r')

# States with CC sites
states = unique(sites$Region)
#states <- c("NC", "MA", "MD", "CT", "MI", "ON", "VA", "SC", "TN", "NY",
#            "GA", "CA", "OR", "UT", "ME", "OH", "WI", "DC", "TX", "RI", "PA", "MO", "IA", "MN", "AK",
#            "AR", "WV", "IN", "NJ", "KY", "AB", "NV", "AL", "NH", "NM")

NAmap <- read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes') %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN"))


# Classify states/provinces
cc_sites <- NAmap %>%
  mutate(
    cc = factor(
      if_else(postal %in% states, "CC site", "No CC site"),
      levels = c("No CC site", "CC site")
    )
  ) %>%
  st_transform(
    crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35"
  )

# Colors corresponding to the factor levels above
cols <- c(
  "No CC site" = "gray95",
  "CC site"    = rgb(93 / 255, 156 / 255, 47 / 255)
)

# Bounding box in the same projected CRS as cc_sites
map_bbox <- st_bbox(
  c(
    xmin = -3200000,
    ymin = -1007822,
    xmax =  3000000,
    ymax =  4500000
  ),
  crs = st_crs(cc_sites)
)

# Plot
cc_map <- tm_shape(cc_sites, bbox = map_bbox) +
  tm_polygons(
    fill = "cc",
    fill.scale = tm_scale_categorical(values = cols),
    fill.legend = tm_legend(show = FALSE),
    col = "gray30",
    lwd = 0.3
  ) +
  tm_layout(frame = FALSE)

cc_map
# Save map as PDF
tmap_save(cc_map, "figs/states_provs_CC_sites_2024.pdf")

