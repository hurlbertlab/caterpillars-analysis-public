# Script for getting the weekly drought level for CC sites, and plotting
# the difference between this year and last year's caterpillar % as a function
# of drought level.

library(sf)

# Drought data downloaded from https://droughtmonitor.unl.edu/Data.aspx
drought <- st_read("data/maps/drought2026/USDM_20260707.shp")

# Check validity
table(st_is_valid(drought))

# Fix invalid geometries
drought <- st_make_valid(drought)

# Check validity
table(st_is_valid(drought))

dat <- read.csv("data/fullDataset_2026-07-07.csv") %>% 
  filter(Year %in% 2025:2026, julianday %in% 152:212) #June and July

# Sites that surveyed in both 2025 and 2026 during June/July
sites <- dat %>%
  distinct(Name, Year) %>%
  count(Name) %>%
  filter(n == 2) %>%
  pull(Name)

dat2 <- dat %>%
  filter(Name %in% sites) %>%
  group_by(Name, Longitude, Latitude, Year) %>%
  summarize(nSurvs = n_distinct(ID),
            nCatSurvs = n_distinct(ID[Group == 'caterpillar']),
            propCat = nCatSurvs/nSurvs) %>%
  arrange(Name, Year)

catdiff <- dat2 %>%
  group_by(Name, Longitude, Latitude) %>%
  summarize(d26.25 = propCat[2] - propCat[1])

pts_sf <- st_as_sf(catdiff, coords = c("Longitude", "Latitude"), crs = 4326)

pts_sf <- st_transform(pts_sf, st_crs(drought))
site_drought <- st_join(pts_sf, drought)

# Assume that every site where Object and DM are NA have no drought
# (exception would be Canadian sites that did not intersect)
table(site_drought$OBJECTID)

site_drought$OBJECTID[is.na(site_drought$OBJECTID) & catdiff$Latitude[catdiff$Name == site_drought$Name] > 48] = 0

table(site_drought$OBJECTID)

# Plotting
dcolors = c('white', 'yellow', 'orange', 'red', 'darkred')

par(mar = c(5, 5, 1, 1))
boxplot(100*site_drought$d26.25 ~ site_drought$OBJECTID, xlab = "Drought Level", 
        ylab = "2026 - 2025 Caterpillar %", col = dcolors, xaxt = "n", cex.lab = 1.5)
mtext(c("Dry", "Moderate", "Severe", "Extreme", "Exceptional"), 1, at = 1:5, cex = 1.25, line = 1)
points(site_drought2$OBJECTID, 100*site_drought2$d26.25, cex = 2)
abline(h = 0, col = 'red', lty = 'dashed', lwd = 4)
