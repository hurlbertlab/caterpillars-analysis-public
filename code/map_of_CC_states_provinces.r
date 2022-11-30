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


cc_sites <- NAmap %>%
  mutate(cc = as.factor(ifelse(postal %in% states, 1, 0))) %>%
  st_transform(crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")

# Set colors for states with/without CC site
cols <- c("gray95", rgb(93/255, 156/255, 47/255))

# Get bounding box coordinates
st_bbox(cc_sites)
#     xmin     ymin     xmax     ymax 
# -6497839 -1007822  3375854  5298100 

# Plot states/provinces shaded by CC site yes/no
cc_map <- tm_shape(cc_sites, bbox=tmaptools::bb(matrix(c(-3200000, -1007822, 3000000, 4500000),2,2))) + 
  tm_polygons(col = "cc", palette = cols, legend.show = F)

# Save map as PDF
tmap_save(cc_map, "figs/states_provs_CC_sites_2022.pdf")

