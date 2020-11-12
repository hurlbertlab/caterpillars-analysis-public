# Examining variation in caterpillar density across host plant species

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

library(grid)
library(tmap)


# What plant species 1) have been surveyed sufficiently in the month of June, 2) across the most sites?
june_plant_surveys = fullDataset %>% 
  filter(substr(LocalDate, 7, 7) == 6) %>% #filter to June
  distinct(Name, PlantSpecies, ID) %>%     #unique survey events
  count(Name, PlantSpecies) %>%
  filter(n >= 20) %>%                      #plant has at least 20 surveys total (in June)
  count(PlantSpecies) %>%                  #number of sites a species meets this 20 survey threshold
  arrange(desc(n))

# American beech, red maple, Great rhododendron, and sugar maple all occur at 9+ sites with this threshold.

june_sites = fullDataset %>% 
  filter(substr(LocalDate, 7, 7) == 6) %>% #filter to June
  distinct(Name, PlantSpecies, ID) %>%     #unique survey events
  count(Name, PlantSpecies) %>%
  filter(n >= 20, 
         PlantSpecies %in% c('American beech', 'Red maple', 'Great rhododendron', 'Sugar maple')) %>%
  arrange(PlantSpecies)


june_hexes = fullDataset %>% 
  filter(substr(LocalDate, 7, 7) == 6) %>% #filter to June
  distinct(cell, PlantSpecies, ID) %>%     #unique survey events
  count(cell, PlantSpecies) %>%
  filter(n >= 20, 
         PlantSpecies %in% c('American beech', 'Red maple', 'Great rhododendron', 'Sugar maple')) %>%
  arrange(PlantSpecies)


# Calculate caterpillar occurrence/density for these site-plant combinations

summaryStats = fullDataset %>%
  filter(substr(LocalDate, 7, 7) == 6,
         PlantSpecies %in% c('American beech', 'Red maple', 'Great rhododendron', 'Sugar maple'),
         Name %in% june_sites$Name) %>%
  group_by(Name, Latitude, Longitude, PlantSpecies, ID, Group) %>% 
  summarize(totalCount = sum(Quantity)) %>%
  group_by(Name, Latitude, Longitude, PlantSpecies) %>%
  summarize(nSurveys = n_distinct(ID),
            nSurveysGTzero = sum(Group == 'caterpillar' & totalCount > 0),
            nCaterpillars = sum(totalCount[Group == 'caterpillar'], na.rm = TRUE),
            catsPerSurvey = nCaterpillars/nSurveys,
            catOccurrence = nSurveysGTzero/nSurveys) %>%
  filter(nSurveys >= 20)
  
beech = filter(summaryStats, PlantSpecies == 'American beech')
red = filter(summaryStats, PlantSpecies =='Red maple')
sugar = filter(summaryStats, PlantSpecies == 'Sugar maple')
rhod = filter(summaryStats, PlantSpecies == 'Great rhododendron')


hexStats = fullDataset %>%
  filter(substr(LocalDate, 7, 7) == 6,
         PlantSpecies %in% c('American beech', 'Red maple', 'Great rhododendron', 'Sugar maple'),
         cell %in% june_hexes$cell) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.))) %>%
  group_by(cell, PlantSpecies, ID, Group) %>% 
  summarize(totalCount = sum(Quantity)) %>%
  group_by(cell, PlantSpecies) %>%
  summarize(nSurveys = n_distinct(ID),
            nSurveysGTzero = sum(Group == 'caterpillar' & totalCount > 0),
            nCaterpillars = sum(totalCount[Group == 'caterpillar'], na.rm = TRUE),
            catsPerSurvey = nCaterpillars/nSurveys,
            catOccurrence = nSurveysGTzero/nSurveys,
            catOccurrencePct = 100*catOccurrence) %>%
  filter(nSurveys >= 20)

beechhex = filter(hexStats, PlantSpecies == 'American beech')
redhex = filter(hexStats, PlantSpecies =='Red maple')
sugarhex = filter(hexStats, PlantSpecies == 'Sugar maple')
rhodhex = filter(hexStats, PlantSpecies == 'Great rhododendron')


            
########################
## ggplot maps

NAmap = read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

easternNA <- NAmap %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  st_crop(c(xmin = -90, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform("+proj=ortho +lon_0=-75 +lat_0=40")

# hex grid
hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")


cats_sf <- hex %>%
  right_join(hexStats) %>%
  st_transform(st_crs(easternNA))

beech_sf <- filter(cats_sf, PlantSpecies == 'American beech')
sugar_sf <- filter(cats_sf, PlantSpecies == 'Sugar maple')
red_sf <- filter(cats_sf, PlantSpecies == 'Red maple')

beech_map <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(beech_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "A. American beech")

sugar_map <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(sugar_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5, outer.margins = c(0.01,0.01,0.01,0.01), title = "B. Sugar maple")

red_map <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(red_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "C. Red maple")


grid.newpage()
pdf(paste0(getwd(),"/figs/caterpillars-count/cat_density_on_3focal_plants_map.pdf"), height = 10, width = 20)
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3)))
print(beech_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(sugar_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(red_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()

# Tree maps with CC site points

# sites points shapefile

sites <- fullDataset %>%
  distinct(Name, Latitude, Longitude, cell)

beech_sites_points <- june_sites %>%
  left_join(sites) %>%
  filter(cell %in% beech_sf$cell) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(easternNA))

beech_map_points <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(beech_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_shape(beech_sites_points) + tm_symbols(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "A. American beech")

sugar_sites_points <- june_sites %>%
  left_join(sites) %>%
  filter(cell %in% sugar_sf$cell) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(easternNA))

sugar_map_points <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(sugar_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_shape(sugar_sites_points) + tm_symbols(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5, outer.margins = c(0.01,0.01,0.01,0.01), title = "B. Sugar maple")

red_sites_points <- june_sites %>%
  left_join(sites) %>%
  filter(cell %in% red_sf$cell) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(easternNA))

red_map_points <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(red_sf) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_shape(red_sites_points) + tm_symbols(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "C. Red maple")


grid.newpage()
pdf(paste0(getwd(),"/figs/caterpillars-count/cat_density_on_3focal_plants_map_withCCsites.pdf"), height = 10, width = 20)
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3)))
print(beech_map_points, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(sugar_map_points, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(red_map_points, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()



##################################
# Considering variation in density by month (because June in GA may reflect something different than June in Ontario)
hexLat = sites3 %>%
  group_by(cell) %>%
  summarize(meanLat = mean(Latitude, na.rm = TRUE),
            meanGreen = mean(medianGreenup, na.rm = TRUE))
hexLat$cell = as.numeric(hexLat$cell)


hexStats2 = fullDataset %>%
  mutate(month = month(LocalDate)) %>%
  filter(PlantSpecies %in% c('American beech', 'Red maple', 'Sugar maple'),
         cell %in% june_hexes$cell) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.))) %>%
  group_by(cell, month, PlantSpecies, ID, Group) %>% 
  summarize(totalCount = sum(Quantity)) %>%
  group_by(cell, month, PlantSpecies) %>%
  summarize(nSurveys = n_distinct(ID),
            nSurveysGTzero = sum(Group == 'caterpillar' & totalCount > 0),
            nCaterpillars = sum(totalCount[Group == 'caterpillar'], na.rm = TRUE),
            catsPerSurvey = nCaterpillars/nSurveys,
            catOccurrence = nSurveysGTzero/nSurveys,
            catOccurrencePct = 100*catOccurrence) %>%
  filter(nSurveys >= 30) %>% 
  left_join(hexLat, by = 'cell')

beech2 = filter(hexStats2, PlantSpecies == 'American beech', month %in% c(6,7))
red2 = filter(hexStats2, PlantSpecies == 'Red maple', month %in% c(6,7))
sugar2 = filter(hexStats2, PlantSpecies == 'Sugar maple', month %in% c(6,7))


par(mfrow = c(3, 1))
plot(c(6,7), range(beech2$catsPerSurvey), xlab = 'Month', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(beech2$cell), function(x) points(beech2$month[beech2$cell == x], beech2$catsPerSurvey[beech2$cell == x], type = 'l', col = round(beech2$meanLat[beech2$cell == x][1], 0), lwd = 2))

legend("topright", legend = unique(round(hexStats2$meanLat)), lwd = 2, col = unique(round(hexStats2$meanLat)), bty = 'n')

plot(c(6,7), range(red2$catsPerSurvey), xlab = 'Month', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(red2$cell), function(x) points(red2$month[red2$cell == x], red2$catsPerSurvey[red2$cell == x], type = 'l', col = round(red2$meanLat[red2$cell == x][1], 0), lwd = 2))

plot(c(6,7), range(sugar2$catsPerSurvey), xlab = 'Month', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(sugar2$cell), function(x) points(sugar2$month[sugar2$cell == x], sugar2$catsPerSurvey[sugar2$cell == x], type = 'l', col = round(sugar2$meanLat[sugar2$cell == x][1], 0), lwd = 2))


# Weekly resolution

hexStatsWeek = fullDataset %>%
  filter(PlantSpecies %in% c('American beech', 'Red maple', 'Sugar maple'),
         cell %in% june_hexes$cell) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.))) %>%
  group_by(cell, julianweek, PlantSpecies, ID, Group) %>% 
  summarize(totalCount = sum(Quantity)) %>%
  group_by(cell, julianweek, PlantSpecies) %>%
  summarize(nSurveys = n_distinct(ID),
            nSurveysGTzero = sum(Group == 'caterpillar' & totalCount > 0),
            nCaterpillars = sum(totalCount[Group == 'caterpillar'], na.rm = TRUE),
            catsPerSurvey = nCaterpillars/nSurveys,
            catOccurrence = nSurveysGTzero/nSurveys,
            catOccurrencePct = 100*catOccurrence) %>%
  filter(nSurveys >= 10) %>% 
  left_join(hexLat, by = 'cell')

beechWeek = filter(hexStatsWeek, PlantSpecies == 'American beech', julianweek %in% 144:200)
redWeek = filter(hexStatsWeek, PlantSpecies == 'Red maple', julianweek %in% 144:200)
sugarWeek = filter(hexStatsWeek, PlantSpecies == 'Sugar maple', julianweek %in% 144:200)

colorpal = rainbow(15)

par(mfrow = c(3, 1), mar = c(4, 4, 1, 1))
plot(c(144,200), range(beechWeek$catsPerSurvey), xlab = 'Week', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(beechWeek$cell), function(x) points(beechWeek$julianweek[beechWeek$cell == x], beechWeek$catsPerSurvey[beechWeek$cell == x], type = 'l', col = colorpal[which(round(beechWeek$meanLat[beechWeek$cell == x][1], 0) == 35:49)], lwd = 2))

legend("topright", legend = 35:49, lwd = 2, col = colorpal, bty = 'n')

plot(c(144,200), range(redWeek$catsPerSurvey), xlab = 'Week', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(redWeek$cell), function(x) points(redWeek$julianweek[redWeek$cell == x], redWeek$catsPerSurvey[redWeek$cell == x], type = 'l', col = colorpal[which(round(redWeek$meanLat[redWeek$cell == x][1], 0) == 35:49)], lwd = 2))

plot(c(144,200), range(sugarWeek$catsPerSurvey), xlab = 'Week', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(sugarWeek$cell), function(x) points(sugarWeek$julianweek[sugarWeek$cell == x], sugarWeek$catsPerSurvey[sugarWeek$cell == x], type = 'l', col = colorpal[which(round(sugarWeek$meanLat[sugarWeek$cell == x][1], 0) == 35:49)], lwd = 2))

# Shift by meanGreen
par(mfrow = c(3, 1), mar = c(4, 4, 1, 1))
plot(c(0,100), range(beechWeek$catsPerSurvey), xlab = 'Days after green-up', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(beechWeek$cell), function(x) points(beechWeek$julianweek[beechWeek$cell == x] - beechWeek$meanGreen[beechWeek$cell == x][1], 
         beechWeek$catsPerSurvey[beechWeek$cell == x], type = 'l', col = colorpal[which(round(beechWeek$meanLat[beechWeek$cell == x][1], 0) == 35:49)], lwd = 2))

legend("topright", legend = 35:49, lwd = 2, col = colorpal, bty = 'n')

plot(c(0,100), range(redWeek$catsPerSurvey), xlab = 'Days after green-up', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(redWeek$cell), function(x) points(redWeek$julianweek[redWeek$cell == x]  - redWeek$meanGreen[redWeek$cell == x][1], redWeek$catsPerSurvey[redWeek$cell == x], type = 'l', col = colorpal[which(round(redWeek$meanLat[redWeek$cell == x][1], 0) == 35:49)], lwd = 2))

plot(c(0,100), range(sugarWeek$catsPerSurvey), xlab = 'Days after green-up', las = 1, ylab = 'Caterpillar density', type = 'n')
sapply(unique(sugarWeek$cell), function(x) points(sugarWeek$julianweek[sugarWeek$cell == x] - sugarWeek$meanGreen[sugarWeek$cell == x][1], sugarWeek$catsPerSurvey[sugarWeek$cell == x], type = 'l', col = colorpal[which(round(sugarWeek$meanLat[sugarWeek$cell == x][1], 0) == 35:49)], lwd = 2))


###########################################################################################
# Getting caterpillar density in window 30-60 days post medianGreenup (no data for Ontario)

hexStatsGreenup = fullDataset %>%
  filter(PlantSpecies %in% c('American beech', 'Red maple', 'Sugar maple'), 
         julianday >= medianGreenup + 30, julianday <= medianGreenup + 60) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.))) %>%
  group_by(cell, PlantSpecies, ID, Group) %>% 
  summarize(totalCount = sum(Quantity)) %>%
  group_by(cell, PlantSpecies) %>%
  summarize(nSurveys = n_distinct(ID),
            nSurveysGTzero = sum(Group == 'caterpillar' & totalCount > 0),
            nCaterpillars = sum(totalCount[Group == 'caterpillar'], na.rm = TRUE),
            catsPerSurvey = nCaterpillars/nSurveys,
            catOccurrence = nSurveysGTzero/nSurveys,
            catOccurrencePct = 100*catOccurrence) %>%
  filter(nSurveys >= 20) %>% 
  left_join(hexLat, by = 'cell')


cats_sf_green <- hex %>%
  right_join(hexStatsGreenup) %>%
  st_transform(st_crs(easternNA))

beech_sf_green <- filter(cats_sf_green, PlantSpecies == 'American beech')
sugar_sf_green <- filter(cats_sf_green, PlantSpecies == 'Sugar maple')
red_sf_green <- filter(cats_sf_green, PlantSpecies == 'Red maple')

beech_map_green <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(beech_sf_green) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "A. American beech")

sugar_map_green <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(sugar_sf_green) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5, outer.margins = c(0.01,0.01,0.01,0.01), title = "B. Sugar maple")

red_map_green <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(red_sf_green) + tm_polygons(col = "catsPerSurvey", palette = "YlGnBu", title = "Density", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2.5, title.size = 2.5,  outer.margins = c(0.01,0.01,0.01,0.01), title = "C. Red maple")


grid.newpage()
pdf(paste0(getwd(),"/figs/caterpillars-count/cat_density_on_3focal_plants_map_green_30-60d_post_greenup.pdf"), height = 10, width = 20)
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3)))
print(beech_map_green, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(sugar_map_green, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(red_map_green, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()

