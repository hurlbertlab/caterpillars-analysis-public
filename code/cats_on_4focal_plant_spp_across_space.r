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



###################
# Old school

library(maps)

pdf('figs/caterpillars-count/cats_on_beech_and_maples_in_June.pdf', height = 6, width = 12)
par(mfrow = c(1, 3), mar = c(1, 0, 3, 0))

map('state', xlim = c(-90,-66))
points(beech$Longitude, beech$Latitude, pch = 16, col = 'orange1', cex = 30*beech$catOccurrence)
points(beech$Longitude, beech$Latitude, pch = 1, cex = 30*beech$catOccurrence)
points(beech$Longitude[beech$catOccurrence == 0], beech$Latitude[beech$catOccurrence == 0], col = 'orange1', pch = 4, cex = 1.8)
mtext('American beech', 3, cex = 1.5)

map('state', xlim = c(-90,-66))
points(sugar$Longitude, sugar$Latitude, pch = 16, col = 'limegreen', cex = 30*sugar$catOccurrence)
points(sugar$Longitude, sugar$Latitude, pch = 1, cex = 30*sugar$catOccurrence)
points(sugar$Longitude[sugar$catOccurrence == 0], sugar$Latitude[sugar$catOccurrence == 0], col = 'limegreen', pch = 4, cex = 1.8)
mtext('Sugar maple', 3, cex = 1.5)

points(rep(-73, 4), c(33, 31, 29, 27), pch = c(4, 16, 16, 16), cex = c(1.8, 30*c(.04, .12, .2)))
text(rep(-70, 4), c(33, 31, 29, 27), pch = c(4, 16, 16, 16), c('0%', '4%', '12%', '20%'), cex = 1.5)  

map('state', xlim = c(-90,-66))
points(red$Longitude, red$Latitude, pch = 16, col = 'red', cex = 30*red$catOccurrence)
points(red$Longitude, red$Latitude, pch = 1, cex = 30*red$catOccurrence)
points(red$Longitude[red$catOccurrence == 0], red$Latitude[red$catOccurrence == 0], col = 'red', pch = 4, cex = 1.8)
mtext('Red maple', 3, cex = 1.5)
dev.off()



map('state', xlim = c(-90,-60))
points(rhod$Longitude, rhod$Latitude, pch = 16, col = 'dodgerblue', cex = 20*rhod$catOccurrence)
points(rhod$Longitude[rhod$catOccurrence == 0], rhod$Latitude[rhod$catOccurrence == 0], col = 'dodgerblue', pch = 4)


