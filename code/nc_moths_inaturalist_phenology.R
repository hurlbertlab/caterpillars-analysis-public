### Modeling phenology across multiple data sources

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)
library(zoo)
library(mgcv)
library(raster)

# iNaturalist, four families, combine
# Filter out caterpillar records 

inat_moths <- read.csv("data/inat_moths.csv", header = T)

inat_cats = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

inat_species <- read.table("data/inat_species.txt", header = T, sep = "\t", quote = '\"')

# Remove caterpillar observations
inat_adults <- inat_moths %>%
  filter(!(id %in% inat_cats$id))

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

###### Caterpillars: iNaturalist vs. Caterpillars Count

# This is located in code/compare_inat_cc.R

###### Adult moths: NC Moths vs. iNaturalist #########

# NC Moths

mnc_species <- read.table("data/mnc_species_complete.txt", header = T)
mnc <- read.csv("\\\\BioArk\\HurlbertLab\\Databases\\NC Moths\\moth_records_thru2018_lineCleaned.txt", header=T, sep = ';', stringsAsFactors = F)

# Map of data records at county level for four major woody moth families

NC <- st_read("data/CountyBoundary/BoundaryCountyPolygon.shp") %>%
  dplyr::select(CountyName, geometry)
NC_geog = st_transform(NC, "+proj=longlat +datum=WGS84")
NC_centroids = st_centroid(NC_geog) %>% 
  st_coordinates() %>%
  data.frame() %>%
  mutate(CountyName = NC$CountyName)

regions <- mnc %>%
  distinct(region, county)
regions$region <- regions$region %>% fct_collapse(Mountains = c("Low Mountains", "High Mountains"), Piedmont = c("P", "Piedmont"))

regions_cleaned <- regions %>%
  mutate(region = as.character(region), 
         county = str_to_title(county)) %>%
  filter(region != "")

years <- c(2016:2018)
for(yr in years){
  mnc_filtered <- mnc %>%
    left_join(mnc_species, by = c("sciName" = "sci_name")) %>%
    filter(family == "Geometridae" | family == "Erebidae" | family == "Noctuidae" | family == "Notodontidae") %>%
    filter(grepl("UV", method)) %>%
    mutate(year = year(as.Date(date)), jday = yday(as.Date(date))) %>%
    left_join(NC_centroids, by = c('county' = 'CountyName')) %>%
    rename('lat_old' = 'lat', 'lon_old' = 'lon', 'lat' = 'Y', 'lon' = 'X') %>%
    filter(year == yr)
  
  mnc_survey_effort <- mnc_filtered %>%
    group_by(county) %>%
    summarize(moths = sum(as.numeric(number))) %>%
    rename("CountyName" = "county")
  
  mnc_map <- merge(NC, mnc_survey_effort, by = "CountyName")
  
  mnc_tm <- tm_shape(NC) + tm_borders() + tm_shape(mnc_map) + 
    tm_fill(col = "moths", palette = "BuGn", title = "Number of Moths") + 
    tm_shape(NC) + tm_borders() +
    tm_layout(title = as.character(yr), legend.text.size = 1, legend.title.size = 1.5)
  mnc_tm
#  tmap_save(mnc_tm, paste0("figs/moths-nc/NC_moths_countyMap_", as.character(yr), ".pdf"))

# NC iNaturalist obs

nc_border <- st_union(NC)

nc_inat <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == yr) %>%
  filter(latitude > 33, latitude < 38, longitude > -83, longitude < -75) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(NC)) %>%
  filter(st_intersects(x = ., y = nc_border, sparse = F))

inat_nc_tmap <- tm_shape(NC) + tm_borders() + tm_shape(nc_inat) + tm_dots() +
  tm_layout(title = as.character(yr))
inat_nc_tmap
#tmap_save(inat_nc_tmap, paste0("figs/inaturalist/NC_iNat_adultMoths_Map_", as.character(yr), ".pdf"))

# Phenology curves for these two, group by 1 degree lat lon bins
# Threshold: 30 data points per spatial bin, march through june (jday 60-181)

begin <- 0
end <- 243
minNumWks <- 10

nc_bins <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year == yr) %>%
  filter(id %in% nc_inat$id) %>%
  mutate(jd_wk = 7*floor(jday/7)) %>%
  left_join(regions_cleaned, by = c("place_county_name" = "county")) %>%
  group_by(region, jd_wk) %>%
  summarize(iNat_moths = n()) %>%
  group_by(region) %>%
  filter(sum(iNat_moths) > 10)

mnc_bins <- mnc_filtered %>%
  mutate(jd_wk = 7*floor(jday/7), 
         county = str_to_title(county)) %>%
  dplyr::select(-region) %>%
  left_join(regions_cleaned) %>%
  group_by(region, jd_wk) %>%
  summarize(MNC_moths = sum(as.numeric(number), na.rm = T)) %>%
  group_by(region) %>%
  filter(sum(MNC_moths) > 10)

nc_moths_spread <- mnc_bins %>%
  full_join(nc_bins, by = c("region", "jd_wk")) %>%
  filter(jd_wk >= begin, jd_wk <= end) %>%
  group_by(region) %>%
  filter(n_distinct(jd_wk) > minNumWks)

nc_moths_comb <- mnc_bins %>%
  full_join(nc_bins, by = c("region", "jd_wk")) %>%
  filter(jd_wk >= begin, jd_wk <= end) %>%
  group_by(region) %>%
  filter(n_distinct(jd_wk) > minNumWks) %>%
  group_by(region) %>%
  filter(sum(MNC_moths, na.rm = T) > 10) %>%
  gather(key = 'data_source', value = "nMoths", MNC_moths, iNat_moths)


# Raw correlations of phenology

ggplot(nc_moths_comb, aes(x = jd_wk, y = nMoths, col = data_source)) +
  geom_line(cex = 1) + 
  scale_color_manual(values=c("deepskyblue3", "springgreen3"), 
                     labels = c("iNat_moths" = "iNaturalist", "MNC_moths" = "Moths NC")) +
  scale_y_log10() +
  facet_wrap(~fct_relevel(region, "Mountains", "Piedmont", "Coastal Plain")) +
  scale_x_continuous(breaks = jds, labels = dates) +
  labs(x = "", y = "Number of Moths", col = "Data source", title = as.character(yr)) +
  theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 14), legend.title = element_text(size = 14),
        strip.text = element_text(size = 15), panel.spacing = unit(1, "lines"))
ggsave(paste0("figs/cross-comparisons/phenocurves_iNat_NCMoths_", as.character(yr), ".pdf"), units = "in", width = 14, height = 5)
}

# Correlation of deltas

nc_moth_deltas <- nc_moths_spread %>%
group_by(lat_bin, lon_bin) %>%
  nest() %>%
  mutate(deltasDF = purrr::map(data, ~{
    df <- .
    len <- nrow(df) - 1
    deltaJD <- c()
    deltaNC <- c()
    deltaiNat <- c()
    for(i in 1:len) {
      JD <- df[i + 1, 1]$jd_wk - df[i, 1]$jd_wk
      NC <- df[i + 1, 2]$MNC_moths - df[i, 2]$MNC_moths
      iNat <- df[i + 1, 3]$iNat_moths - df[i, 3]$iNat_moths
      
      deltaJD <- c(deltaJD, JD)
      deltaNC <- c(deltaNC, NC)
      deltaiNat <- c(deltaiNat, iNat)
    }
    data.frame(deltaJD = deltaJD, deltaNC = deltaNC, deltaiNat = deltaiNat)
  })) %>%
  dplyr::select(lat_bin, lon_bin, deltasDF) %>%
  unnest()

ggplot(nc_moth_deltas, aes(x = deltaNC, y = deltaiNat)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, lty = 2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(lat_bin ~ lon_bin) + theme_bw()
ggsave("figs/cross-comparisons/pheno_deltas_NCMoths_iNat.pdf")

###### Caterpillars <-> Adults: iNaturalist vs. iNaturalist ########

begin <- 0
end <- 264
minNumWks <- 10

adult_moths <- inat_adults %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, year, jd_wk) %>%
  summarize(moths = n()) %>%
  filter(jd_wk >= begin, jd_wk <= end) %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(n_distinct(jd_wk) >= minNumWks)

caterpillars <- inat_cats %>%
  left_join(inat_species, by = "scientific_name") %>%
  filter(family %in% c("Erebidae", "Geometridae", "Noctuidae", "Notodontidae")) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  group_by(lat_bin, lon_bin, year, jd_wk) %>%
  summarize(caterpillars = n()) %>%
  filter(jd_wk >= begin, jd_wk <= end) %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(n_distinct(jd_wk) >= minNumWks)

inat_combined <- adult_moths %>%
  full_join(caterpillars, by = c("lat_bin", "lon_bin", "year", "jd_wk")) %>%
  filter(lat_bin > 24)

inat_combined_gather <- inat_combined %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(sum(caterpillars, na.rm = T) > 50) %>%
  gather(key = 'life_stage', value = "nObs", moths, caterpillars) 

# filter, no more thank 1 wk gaps Apr-Jun
missing_data <- inat_combined %>%
  arrange(lat_bin, lon_bin, year, jd_wk) %>%
  group_by(lat_bin, lon_bin, year) %>%
  nest() %>%
  mutate(cats_inter = map(data, ~{na.approx(.$caterpillars, maxgap = 1, na.rm = F)}),
         moths_inter = map(data, ~{na.approx(.$moths, maxgap = 1, na.rm = F)})) %>%
  unnest() %>%
  filter(jd_wk >= 91, jd_wk <= 181) %>%
  group_by(lat_bin, lon_bin, year) %>%
  summarize(sumCats = sum(cats_inter),
            sumMoths = sum(moths_inter)) %>%
  group_by(year) %>%
  summarize(nBins = sum(!is.na(sumCats))) ### 16 in 2018, 8 in 2017

# Plot of geographic extent of data
box <- c(xmin = -105, xmax = -65, ymin = 25, ymax = 50)

northAM <- read_sf("data/maps/ne_50m_admin_1_states_provinces_lakes.shp")

eNA <- st_crop(northAM, box)

raster_iNat <- function(yr, lifestage) {
  inat_to_raster <- inat_combined_gather %>%
    group_by(lat_bin, lon_bin, year, life_stage) %>%
    summarize(obs = sum(nObs, na.rm = T)) %>%
    filter(year == yr, life_stage == lifestage) %>%
    ungroup() %>%
    dplyr::select(lon_bin, lat_bin, obs, -year, -life_stage)
  rasterFromXYZ(inat_to_raster, crs = crs(eNA))
}

cat18_raster <- raster_iNat(2018, "caterpillars")
cat17_raster <- raster_iNat(2017, "caterpillars")
moth18_raster <- raster_iNat(2018, "moths")
moth17_raster <- raster_iNat(2017, "moths")

cat18_map <- tm_shape(eNA) + tm_polygons(fill = "gray") + tm_shape(cat18_raster) + 
  tm_raster(palette = "BuPu", title = "Caterpillar obs") + 
  tm_shape(eNA) + tm_borders() +
  tm_layout(title = "2018", legend.position = c("right", "bottom"))

cat17_map <- tm_shape(eNA) + tm_polygons(fill = "gray") + tm_shape(cat17_raster) + 
  tm_raster(palette = "BuPu", legend.show = F) + 
  tm_shape(eNA) + tm_borders() +
  tm_layout(title = "2017")

moth18_map <- tm_shape(eNA) + tm_polygons(fill = "gray") + tm_shape(moth18_raster) + 
  tm_raster(palette = "BuGn", title = "Moth obs") + 
  tm_shape(eNA) + tm_borders() +
  tm_layout(title = "2018", legend.position = c("right", "bottom"))

moth17_map <- tm_shape(eNA) + tm_polygons(fill = "gray") + tm_shape(moth18_raster) + 
  tm_raster(palette = "BuGn", legend.show = F) + 
  tm_shape(eNA) + tm_borders() +
  tm_layout(title = "2017")

inat_maps <- tmap_arrange(cat17_map, cat18_map, moth17_map, moth18_map, nrow = 2)
#tmap_save(inat_maps, "figs/inaturalist/data_map.pdf", units = "in", height = 6, width = 10)

# Phenology curves, group by 2 degree lat lon bins

bins <- inat_combined_gather %>%
  distinct(lat_bin, lon_bin)
bins$group <- row.names(bins)

inat_combined_ids <- inat_combined_gather %>%
  left_join(bins, by = c("lat_bin", "lon_bin", "year"))

# 10% accumulation

accum_date <- inat_combined_ids %>%
  replace_na(list(nObs = 0)) %>%
  group_by(lat_bin, lon_bin, year, life_stage) %>%
  arrange(jd_wk) %>%
  mutate(total = sum(nObs, na.rm = T), ten_percent = 0.1*total, accum = cumsum(nObs)) %>%
  group_by(lat_bin, lon_bin, year, life_stage) %>%
  filter(accum >= ten_percent) %>%
  filter(jd_wk == min(jd_wk)) %>%
  rename(accum_wk = jd_wk)

inat_combined_accum <- inat_combined_ids %>%
  left_join(dplyr::select(accum_date, lat_bin, lon_bin, year, life_stage, accum_wk),
            by = c("lat_bin", "lon_bin", "year", "life_stage"))

# Raw correlations of phenology
for(yr in c(2015:2018)) {
  ggplot(dplyr::filter(inat_combined_accum, year == yr, !is.na(nObs)), aes(x = jd_wk, y = nObs, col = life_stage)) +
    geom_line(cex = 1) + 
    scale_color_manual(values=c("deepskyblue3", "springgreen3"), 
                       labels = c("caterpillars" = "Caterpillars", "moths" = "Moths")) +
    scale_y_log10() +
    facet_grid(forcats::fct_reorder(factor(lat_bin), desc(lat_bin))~lon_bin) +
    scale_x_continuous(breaks = jds, labels = dates) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          strip.text = element_text(size = 20), legend.text = element_text(size = 20),
          legend.title = element_text(size = 20), axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    labs(x = "", y = "Number of observations", col = "Life stage")
  ggsave(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_", yr, ".pdf"), units = "in", height = 20, width = 30)
  
  bins_yr <- bins %>%
    dplyr::filter(year == yr)
  
  pdf(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_pages_", yr, ".pdf"), height = 5, width = 8)
  for(i in bins_yr$group) {
    df <- inat_combined_accum %>%
      dplyr::filter(year == yr) %>%
      dplyr::filter(group == i, !is.na(nObs)) %>%
      group_by(lat_bin, lon_bin, life_stage, accum_wk) %>%
      mutate(n = sum(nObs, na.rm = T))
    nmoths <- unique(df$n)[[1]]
    ncats <- unique(df$n)[[2]]
    location <- paste0(unique(df$lat_bin), ", ", unique(df$lon_bin))
    plot <- ggplot(df, aes(x = jd_wk, y = nObs, col = life_stage)) +
      geom_line(cex = 1) + 
      scale_color_manual(values=c("deepskyblue3", "springgreen3"), 
                         labels = c("caterpillars" = "Caterpillars", "moths" = "Moths")) +
      scale_y_log10() +
      scale_x_continuous(breaks = jds, labels = dates) +
      geom_vline(aes(xintercept = accum_wk, col = life_stage), lty = 2, show.legend = F) +
      labs(x = "", y = "Number of observations", col = "Life stage") +
      theme(legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15), 
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15)) +
      ggtitle(location)
    plot2 <- plot_grid(plot, labels = c(paste0("Caterpillars = ", as.character(ncats))),
                       label_x = c(0.69), label_y = c(0.3))
    plot3 <- plot_grid(plot2, labels = c(paste0("Moths = ", as.character(nmoths))),
                       label_x = c(0.72), label_y = c(0.25))
    print(plot3)
  }
  dev.off()
  
}

## Fit GAM, obtain inflection point from model fit

inat_gams <- inat_combined %>%
  group_by(lat_bin, lon_bin, year) %>%
  nest() %>%
  mutate(n_cat = map_dbl(data, ~{
    df <- .
    length(na.omit(df$caterpillars))
  }), n_moth = map_dbl(data, ~{
    df <- .
    length(na.omit(df$moths))
  })) %>%
  filter(n_cat > 0, n_moth > 0) %>%
  mutate(cat_interp = map(data, ~{
    df <- .
    df$caterpillars <- na.approx(df$caterpillars, maxgap = 1, na.rm = F)
    df
  }), moth_interp = map(data, ~{
    df <- .
    df$moths <- na.approx(df$moths, maxgap = 1, na.rm = F)
    df
  })) %>%  
  mutate(gam_cat = map(cat_interp, ~{
    df <- .
    gam(caterpillars ~ s(jd_wk), data = df)
  }), gam_moth = map(moth_interp, ~{
    df <- .
    gam(moths ~ s(jd_wk), data = df)
  })) %>%
  mutate(cat_r2 = map_dbl(gam_cat, ~{
  df <- summary(.)
  df$r.sq
  }), moth_r2 = map_dbl(gam_moth, ~{
  df <- summary(.)
  df$r.sq
  })) %>% 
  mutate(cat_predict = map(gam_cat, ~{
    predict(., na.action = na.exclude())
  }), moth_predict = map(gam_moth, ~{
    predict(., na.action = na.exclude())
  })) %>%
  dplyr::select(-n_cat, -n_moth, -gam_cat, -gam_moth) %>%
  mutate(cat_dates = map(cat_interp, ~{
    df <- .
    cats <- df[, -2]
    na.omit(cats)
  }),
  moth_dates = map(moth_interp, ~{
    df <- .
    moths <- df[, -3]
    na.omit(moths)
  }))

cat_gams <- inat_gams %>%
  dplyr::select(lat_bin, lon_bin, year, cat_r2, cat_predict, cat_dates) %>%
  unnest() %>%
  rename("r2" = "cat_r2", "predict" = "cat_predict", "nObs" = "caterpillars") %>%
  mutate(life_stage = "caterpillars")

moth_gams <- inat_gams %>%
  dplyr::select(lat_bin, lon_bin, year, moth_r2, moth_predict, moth_dates) %>%
  unnest() %>%
  rename("r2" = "moth_r2", "predict" = "moth_predict", "nObs" = "moths") %>%
  mutate(life_stage = "moths")

gams_all <- bind_rows(cat_gams, moth_gams)

bins_gams <- gams_all %>%
  distinct(lat_bin, lon_bin, year)
bins_gams$group <- row.names(bins_gams)

gams_ids <- gams_all %>%
  left_join(bins_gams, by = c("lat_bin", "lon_bin", "year"))

gams_gather <- gams_ids %>%
  left_join(dplyr::select(accum_date, lat_bin, lon_bin, year, accum_wk, life_stage)) %>%
  filter(!(is.na(accum_wk)))
  
gams_accum <- gams_gather %>%  
  group_by(lat_bin, lon_bin, year, life_stage) %>%
  arrange(lat_bin, lon_bin, year, life_stage, jd_wk) %>%
  mutate(total_gam = sum(predict, na.rm = T), 
         ten_percent_gam = 0.1*total_gam, 
         accum = cumsum(ifelse(is.na(predict), 0, predict))) %>%
  group_by(lat_bin, lon_bin, year, life_stage) %>%
  filter(accum >= ten_percent_gam) %>%
  filter(jd_wk == min(jd_wk)) %>%
  rename(accum_gam = jd_wk) %>%
  dplyr::select(lat_bin, lon_bin, year, accum_gam, life_stage)

gams_gather_accum <- gams_gather %>%
  left_join(gams_accum, by = c("lat_bin", "lon_bin", "year", "life_stage"))

# two models: catdate ~ mothdate*lat + year
# catdate - mothdate ~ lat + year

mod_dates <- accum_date %>%
  dplyr::select(lat_bin, lon_bin, group, year, accum_wk, life_stage) %>%
  spread(key = life_stage, value = accum_wk) %>%
  mutate(diff = (caterpillars - moths)/7)

cat_date <- lm(caterpillars ~ moths*lat_bin + year, data = mod_dates)
summary(cat_date)

diff_date <- lm(caterpillars - moths ~ lat_bin + year, data = mod_dates)
summary(diff_date)

mod_dates$predict_cat <- predict(cat_date)
mod_dates$predict_diff <- predict(diff_date)

ggplot(mod_dates, aes(x = moths, y = predict_cat, group = lat_bin, col = factor(lat_bin))) + geom_line(cex = 1) +
  facet_wrap(~year) + labs(x = "Moth date", y = "Predicted caterpillar date", col = "Latitude") + theme_bw() +
  theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12), axis.title = element_text(size = 12), strip.text = element_text(size = 12))
#ggsave("figs/inaturalist/cat_date_model_predict.pdf")

ggplot(mod_dates, aes(x = lat_bin, y = predict_diff, group = year, col = factor(year))) + geom_line(cex = 1) +
  labs(x = "Latitude", y = "Predicted difference (caterpillar date - moth date)", col = "Year")
#ggsave("figs/inaturalist/diff_date_model_predict.pdf")

### Cross-correlation analysis

# write function for cross-correlations

cross_cor <- function(time_series) {
  moths <- data.frame(jd_wk = time_series$jd_wk, moths = time_series$moths)
  cats <- data.frame(jd_wk = time_series$jd_wk, cats = time_series$caterpillars)
  results <- data.frame(lag = c(0:10), r = c(NA), nobs = c(NA))
  for(lag in results$lag) {
    cats$jd_wk <- cats$jd_wk - lag*7
    lag_series <- moths %>%
      left_join(cats, by = "jd_wk")
    results[results$lag == lag, 2] <- cor(lag_series$moths, lag_series$cats, use = "pairwise.complete.obs")
    results[results$lag == lag, 3] <- nrow(lag_series)/(38-lag)
  }
  return(results)
}

gams_cross_cor <- gams_all %>%
  dplyr::select(-nObs, -r2) %>%
  spread(life_stage, predict) %>%
  group_by(lat_bin, lon_bin, year) %>%
  arrange(jd_wk) %>%
  nest() %>%
  mutate(cross_corr_gam = map(data, ~{
    cross_cor(.)
  })) %>%
  dplyr::select(-data) %>%
  unnest()  %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(r == max(r, na.rm = T)) %>%
  filter(nobs > 0.2) %>%
  rename("lag_gam" = "lag", "r_gam" = "r", "nobs_gam" = "nobs")

inat_cross <- inat_combined %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(sum(caterpillars, na.rm = T) > 50) %>%
  group_by(lat_bin, lon_bin, year) %>%
  arrange(jd_wk) %>%
  nest() %>%
  mutate(interp = map(data, ~{
    df <- .
    moth <- na.approx(df$moths, maxgap = 1)
    diff_moth <- nrow(df) - length(moth)
    int <- na.approx(df$caterpillars, maxgap = 1)
    diff <- nrow(df) - length(int)
    data.frame(jd_wk = df$jd_wk, moths = c(rep(NA, diff_moth), moth), caterpillars = c(rep(NA, diff), int))
  })) %>%
  mutate(cross_corr = map(interp, ~{
    cross_cor(.)
  })) %>%
  dplyr::select(-data, -interp) %>%
  unnest() 

# best lag distances for each bin

inat_lags <- inat_cross %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(r == max(r, na.rm = T)) %>%
  filter(nobs > 0.2)  %>%
  left_join(dplyr::select(mod_dates, lat_bin, lon_bin, year, diff), by = c("lat_bin", "lon_bin", "year"))

# Cross-correlation plots 

ggplot(inat_lags, aes(x = diff, y = lag, color = lat_bin)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(x = "Difference in 10% accum dates", y = "Best fit lag", col = "Latitude")
ggsave("figs/inaturalist/lags_vs_accum.pdf")

ggplot(inat_lags, aes(x = lat_bin, y = lag, color = factor(lon_bin))) + geom_point() + facet_wrap(~year) + 
  labs(x = "Latitude", y = "Best fit lag (weeks)", color = "Longitude")
ggsave("figs/inaturalist/best_lags.pdf")

inat_lags_means <- inat_cross %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(r == max(r, na.rm = T)) %>%
  filter(nobs > 0.2) %>%
  group_by(lat_bin, year) %>%
  summarize(meanLag = mean(lag))

ggplot(inat_lags_means, aes(x = lat_bin, y = meanLag)) + geom_point() + facet_wrap(~year) + theme_bw() + 
  labs(x = "Latitude", y = "Mean best fit lag (weeks)", color = "Longitude")
ggsave("figs/inaturalist/mean_best_lags.pdf")

# Combine multiple pheno metrics

cross_corr_lags <- inat_lags %>%
  dplyr::select(-nobs, -diff) %>%
  left_join(gams_cross_cor) %>%
  dplyr::select(-nobs_gam)

pheno_metrics <- gams_gather_accum %>%
  left_join(cross_corr_lags, by = c("lat_bin", "lon_bin", "year"))

# Multiple pheno metrics plots
## Things to add: cross_correlation on gams

for(yr in c(2015:2018)) {
  bins_yr <- bins_gams %>%
    filter(year == yr, group %in% pheno_metrics$group)
  
  pdf(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_pages_phenometrics_", yr, ".pdf"), height = 5, width = 8)
  for(i in bins_yr$group) {
    df <- pheno_metrics %>%
      dplyr::filter(year == yr) %>%
      dplyr::filter(group == i, !is.na(nObs)) %>%
      group_by(lat_bin, lon_bin, life_stage, r2, accum_wk, accum_gam, lag, r) %>%
      mutate(n = sum(round(nObs), na.rm = T)) %>%
      mutate(gam = paste0("GAM ", life_stage))
    nmoths <- unique(df$n)[[2]]
    ncats <- unique(df$n)[[1]]
    diffs <- df %>%
      ungroup() %>%
      dplyr::select(life_stage, accum_wk, accum_gam) %>%
      distinct()
    accum_lag <- (diffs$accum_wk[1] - diffs$accum_wk[2])/7
    gam_lag <- (diffs$accum_gam[1] - diffs$accum_gam[2])/7
    moth_r2 <- df %>% filter(life_stage == "moths") %>% distinct(r2)
    cat_r2 <- df %>% filter(life_stage == "caterpillars") %>% distinct(r2)
    location <- paste0(unique(df$lat_bin), ", ", unique(df$lon_bin))
    plot <- ggplot(df, aes(x = jd_wk, y = nObs, col = life_stage)) +
      geom_line(cex = 1) + 
      scale_color_manual(values=c("deepskyblue3", "skyblue1", "palegreen1", "springgreen3"), 
                         labels = c("caterpillars" = "Caterpillars", 
                                    "moths" = "Moths",                                     
                                    "GAM caterpillars" = "GAM Cats", 
                                    "GAM moths" = "GAM Moths")) +
      geom_line(aes(y = predict, col = gam), cex = 1) +
      scale_x_continuous(breaks = jds, labels = dates, limits = c(0, 264)) +
      scale_y_log10() + 
      geom_segment(aes(x = accum_wk, xend = accum_wk, y = 0.75, yend = 0, col = life_stage),
                   size = 1, arrow = arrow(), show.legend = F) +
      geom_segment(aes(x = accum_gam, xend = accum_gam, y = 0.5, yend = 0, col = gam),
                   size = 1, lty = 2, arrow = arrow(), show.legend = F) + 
      labs(x = "", y = "Number of observations", col = "Life stage") +
      theme(legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15), 
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15)) +
      ggtitle(location) +
      annotate("text", x = 21, y = max(df$nObs) - 0.75*max(df$nObs), 
               label = paste0("Cor. lag = ", unique(df$lag), 
                              "\n", "r = ", round(unique(df$r), 2),
                              "\n", "Cor. lag GAM = ", min(unique(df$lag_gam)),
                              "\n", "r GAM = ", round(unique(df$r_gam), 2),
                              "\n", "10% lag = ", accum_lag,
                              "\n", "  10% GAM lag = ", gam_lag,
                              "\n", "    GAM R2 Moth = ", round(moth_r2$r2, 2),
                              "\n", "    GAM R2 Cat = ", round(cat_r2$r2, 2)))
    plot2 <- plot_grid(plot, labels = c(paste0("Caterpillars = ", as.character(ncats))),
                       label_x = c(0.69), label_y = c(0.3))
    plot3 <- plot_grid(plot2, labels = c(paste0("Moths = ", as.character(nmoths))),
                       label_x = c(0.72), label_y = c(0.25))
    print(plot3) # fix dimensions
  }
  dev.off()
  
}
