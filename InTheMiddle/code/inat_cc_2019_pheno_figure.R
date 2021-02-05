## Comparing CC and iNat phenology peaks and centroids in 2019 with temperature means

### Libraries
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(maps)
library(rgdal)
library(sf)
library(tmap)
library(forcats)
library(dggridR)
library(grid)
library(cowplot)

theme_set(theme_classic(base_size = 23))

### Read in CC data and functions
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

### Read in iNat data, mapping, temperature files

inat = read.csv('data/inat_caterpillars_eastern_NA_5-20-2020.csv', header = TRUE, stringsAsFactors = F)

NAmap = read_sf('data/maps', 'ne_50m_admin_1_states_provinces_lakes')

inat_species = read.table("data/taxonomy/inat_caterpillar_species_traits.txt", header = T, sep = "\t")

hex <- st_read("data/maps/hexgrid_materials/hex_grid_crop.shp", stringsAsFactors= F) %>%
  mutate(cell.num = as.numeric(cell)) %>%
  dplyr::select(-cell) %>%
  rename(cell = "cell.num")

cell_centers <- read.csv("data/maps/hexgrid_materials/hex_grid_cell_centers.csv", stringsAsFactors = F) %>%
  mutate_at(c("cell"), round)

hex_springtemp <- read.csv("data/derived_data/hex_mean_temps.csv", stringsAsFactors = F)

### Site effort summary

site_effort <- data.frame(year = c(2015:2019)) %>%
  mutate(site_effort = purrr::map(year, ~{
    y <- .
    siteEffortSummary(fullDataset, year = y)
  })) %>%
  unnest(cols = c(site_effort))

# CC: use good or 50 surveys weeks from cc_inat_pheno_anomalies filters w/o 2 year minimum

cc_sites <- tibble(minWeeksGoodor50Surveys = c(6)) %>%
  mutate(sites = purrr::map(minWeeksGoodor50Surveys, ~{
    nweeks <- .
    site_effort %>%
      filter(nWeeksGoodor50Surveys >= nweeks, modalSurveyBranches >= 20, year == 2019)
  }))

sites_2019 <- cc_sites$sites[[1]]

hex_cells_2019 <- unique(sites_2019$cell)

## Find 6 week common time windows

surveyThreshold = 0.8            # proprortion of surveys conducted to be considered a good sampling day
minJulianWeek = 135              # May 15 beginning of seasonal window for tabulating # of good weeks
maxJulianWeek = 211              # July 30 - end of seasonal window

site_overlap <- fullDataset %>%
  right_join(sites_2019, by = c("Year" = "year", "Name", "Region", "cell", "Latitude", "Longitude", "medianGreenup")) %>%
  filter(case_when(Name == "UNC Chapel Hill Campus" ~ julianweek >= 121, # for UNC Campus, take out BIO 101 observations in April
                   TRUE ~ TRUE)) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, julianweek, medianGreenup) %>%
  mutate(nSurveysPerWeek = n_distinct(ID),
         nSurveyBranches = n_distinct(PlantFK)) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(good_week = ifelse((julianweek >= minJulianWeek & julianweek <= maxJulianWeek) & 
                              (nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek | nSurveysPerWeek > 50), 1, 0)) %>%
  filter(good_week == 1) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(Start = min(julianweek),
         End = max(julianweek)) %>% 
  group_by(Name, Region, cell, Latitude, Longitude, medianGreenup) %>%
  mutate(maxStart = max(Start),
         minEnd = min(End)) %>%
  filter(julianweek >= maxStart & julianweek <= minEnd) %>%
  group_by(Name, Year, Region, cell, Latitude, Longitude, medianGreenup) %>%
  filter(n_distinct(julianweek) >= 6) 

sites_6weeks <- site_overlap %>%
  ungroup() %>%
  distinct(Name, Year, Region, cell, Latitude, Longitude, medianGreenup, Start, End)

hex_start_end <- sites_6weeks %>%
  group_by(Year, cell) %>%
  summarize(start = min(Start),
            end = max(End)) %>%
  ungroup() %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

## calculate peak date and cenroid date

outlierCount = 10000

pheno_2019 <- site_overlap %>%
  mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
  group_by(Name, Region, cell, Latitude, Longitude, Year, julianweek) %>%
  summarize(nSurveyBranches = n_distinct(PlantFK),
            nSurveys = n_distinct(ID),
            totalCount = sum(Quantity2[Group == "caterpillar"], na.rm = TRUE),
            numSurveysGTzero = length(unique(ID[Quantity > 0 & Group == "caterpillar"])),
            totalBiomass = sum(Biomass_mg[Group == "caterpillar"], na.rm = TRUE)) %>% 
  mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
  mutate(meanDensity = totalCount/nSurveys,
         fracSurveys = 100*numSurveysGTzero/nSurveys,
         meanBiomass = totalBiomass/nSurveys) %>%
  group_by(Name, cell) %>%
  summarize(pctPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                 julianweek[fracSurveys == max(fracSurveys, na.rm = TRUE)][1]),
            pctCentroidDate = sum(julianweek*fracSurveys)/sum(fracSurveys)) %>%
  group_by(cell) %>%
  summarize(avgPeakDate = mean(pctPeakDate),
            avgCentroidDate = mean(pctCentroidDate)) %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

# iNaturalist
# Weekly insect observations by hex cell for the inclusive weeks per hex cell from site_overlap

inat_insects <- read.csv("data/derived_data/inat_2019_weekly_insecta_hex.csv", stringsAsFactors = F) %>%
  filter(year == 2019, cell %in% hex_cells_2019) %>%
  filter(nObs > 50) %>%
  ungroup() %>%
  mutate_at(c("year", "cell"), as.numeric) %>%
  left_join(hex_start_end, by = c("year" = "Year", "cell")) %>%
  filter(jd_wk <= end, jd_wk >= start) %>%
  group_by(cell, year) %>%
  mutate(n_wks = n()) %>%
  filter(n_wks >= 6) # Lose cell 592

## Peak and centroid dates for caterpillars (effort corrected)

inat_cats_pheno <- inat %>%
  filter(user_login != "caterpillarscount") %>%
  mutate(year = as.numeric(substr(observed_on, 1, 4)),
         jday = yday(observed_on), 
         jd_wk = 7*floor(jday/7)) %>%
  filter(year == 2019) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_intersection(hex) %>%
  st_set_geometry(NULL) %>%
  group_by(cell, jd_wk) %>%
  summarize(nCats = n_distinct(id))

inat_pheno <- inat_cats_pheno %>%
  right_join(inat_insects, by = c("cell", "jd_wk")) %>%
  mutate(cat_effort = nCats/nObs) %>%
  group_by(cell, year) %>%
  summarize(peakDate = median(jd_wk[cat_effort == max(cat_effort, na.rm = T)], na.rm = T),
            centroidDate = sum(jd_wk*cat_effort, na.rm = T)/sum(cat_effort, na.rm = T)) 

## Figure 4:

## Plot shapefile
pheno_plot <- hex %>%
  right_join(inat_pheno) %>%
  left_join(pheno_2019) %>%
  left_join(cell_centers) %>%
  left_join(hex_springtemp)

pheno_df <- pheno_plot %>%
  st_set_geometry(NULL)
# write.csv(pheno_df, "./InTheMiddle/data/pheno_2019_cc_inat_plot.csv", row.names =F)

easternNA <- NAmap %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform("+proj=ortho +lon_0=-75 +lat_0=40")

pheno_plot_ortho <- pheno_plot %>%
  st_crop(c(xmin = -100, ymin = 20, xmax = -59, ymax = 51)) %>%
  st_transform(st_crs(easternNA))

cc_sites_ortho <- sites_6weeks %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(easternNA))

## Panel 1
# Hex cells with iNat peak date

inat_peak <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + 
  tm_polygons(col = "peakDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65, breaks = c(140, 155, 170, 185, 200, 215))+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2, inner.margins = c(0.02, 0.02, 0.1, 0.05),
            outer.margins = c(0.01,0,0.01,0), title = "A. iNaturalist - peak date")

## Panel 2
# Hex cells with iNat centroid date

inat_cent <-  tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + tm_polygons(col = "centroidDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2, inner.margins = c(0.02, 0.02, 0.1, 0.05),
            outer.margins = c(0.01,0,0.01,0), title = "D. iNaturalist - centroid date")

## Panel 3
# Hex cells with CC peak date

cc_peak <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + 
  tm_polygons(col = "avgPeakDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65, breaks = c(150, 170, 190, 210, 220))+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2, inner.margins = c(0.02, 0.02, 0.1, 0.05),
            outer.margins = c(0.01,0,0.01,0), title = "B. Caterpillars Count! - peak date")

## panel 3 with cc site dots

cc_peak_dots <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + 
  tm_polygons(col = "avgPeakDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65, breaks = c(150, 170, 190, 210, 220))+
  tm_shape(cc_sites_ortho) + tm_symbols(col = "black", size = 0.3, shape = 1) +
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2, inner.margins = c(0.02, 0.02, 0.1, 0.05),
            outer.margins = c(0.01,0,0.01,0), title = "B. Caterpillars Count! - peak date")

## panel 4
# Hex cells with CC centroid date

cc_cent <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + tm_polygons(col = "avgCentroidDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65)+
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2, inner.margins = c(0.02, 0.02, 0.1, 0.05),
            outer.margins = c(0.01,0,0.01,0), title = "E. Caterpillars Count! - centroid date") 

## Panel 4 with dots

cc_cent_dots <- tm_shape(easternNA) + tm_polygons() +
  tm_shape(pheno_plot_ortho) + tm_polygons(col = "avgCentroidDate", palette = "YlGnBu", title = "Day of year", alpha = 0.65)+
  tm_shape(cc_sites_ortho) + tm_symbols(col = "black", size = 0.3, shape = 1) +  
  tm_layout(legend.text.size = 1.5, legend.title.size = 2, title.size = 2,
            outer.margins = c(0.01,0,0.01,0), title = "E. Caterpillars Count! - centroid date") 


## Panel 5
# Scatterplot comparing inat/cc peak date

theme_set(theme_classic(base_size = 23))

cor.test(pheno_plot_ortho$peakDate, pheno_plot_ortho$avgPeakDate) # r = 0.330, p = 0.352

peak_plot <- ggplot(pheno_plot_ortho, aes(x = peakDate, y = avgPeakDate)) + 
  geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, cex = 1.5, col = "darkgray") +
  geom_abline(slope = 1, intercept = 0, cex = 1.5, col = "darkgray", lty = 2) +
  annotate(geom = "text", x = 160, y = 164, label = c("1:1"), size = 9, angle = 35) +
  annotate(geom = "text", x = 190, y = 155, label = c(expression(italic("r") == 0.33 )), size = 9) +
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "C. Peak date") +
  theme(plot.title = element_text(hjust = -0.3, size = 24))

## Panel 6
# Scatterplot comparing inat/cc centroid date

cor.test(pheno_plot_ortho$centroidDate, pheno_plot_ortho$avgCentroidDate) # r = 0.472, p = 0.169

centroid_plot <- ggplot(pheno_plot_ortho, aes(x = centroidDate, y = avgCentroidDate)) + 
  geom_point(cex = 3) + 
  geom_abline(slope = 1, intercept = 0, cex = 1.5, col = "darkgray", lty = 2) +
  annotate(geom = "text", x = 169, y = 171, label = c("1:1"), size = 9, angle = 15) +
  geom_smooth(method = "lm", se = F, cex = 1.5, col = "darkgray") +
  annotate(geom = "text", x = 182, y = 168, label = c(expression(italic('r') == 0.47)), size = 9) +
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "F. Centroid date") +
  theme(plot.title = element_text(hjust = -0.3, size = 24))

  
## Multipanel fig
grid.newpage()
pdf(paste0(getwd(),"/InTheMiddle/figs/inat_cc_2019_phenometrics.pdf"), height = 10, width = 18)
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 3)))
print(inat_peak, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(inat_cent, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(cc_peak, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(cc_cent, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(peak_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(centroid_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
dev.off()

# ## Multipanel fig with CC site dots
# grid.newpage()
# pdf(paste0(getwd(),"/figs/cross-comparisons/inat_cc_2019_phenometrics_withCCsites.pdf"), height = 10, width = 18)
# pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 3)))
# print(inat_peak, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(inat_cent, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(cc_peak_dots, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
# print(cc_cent_dots, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
# print(peak_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
# print(centroid_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
# dev.off()

