### Modeling phenology across multiple data sources

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)
library(zoo)
library(mgcv)

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
    geom_vline(aes(xintercept = accum_wk, col = life_stage), lty = 2, show.legend = F) +
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
    print(plot3) # fix dimensions
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
  mutate(gam_cat = map(data, ~{
    df <- .
    gam(caterpillars ~ s(jd_wk), data = df)
  }), gam_moth = map(data, ~{
    df <- .
    gam(moths ~ s(jd_wk), data = df)
  })) %>%
  mutate(cat_predict = map(gam_cat, ~{
    predict(.)
  }), moth_predict = map(gam_moth, ~{
    predict(.)
  })) %>%
  dplyr::select(-n_cat, -n_moth, -gam_cat, -gam_moth) %>%
  mutate(cat_dates = map(data, ~{
    df <- .
    cats <- df[, -2]
    na.omit(cats)
  }),
  moth_dates = map(data, ~{
    df <- .
    moths <- df[, -3]
    na.omit(moths)
  }))

cat_gams <- inat_gams %>%
  dplyr::select(lat_bin, lon_bin, year, cat_predict, cat_dates) %>%
  unnest()

moth_gams <- inat_gams %>%
  dplyr::select(lat_bin, lon_bin, year, moth_predict, moth_dates) %>%
  unnest()

gams_all <- full_join(cat_gams, moth_gams)

bins_gams <- gams_all %>%
  distinct(lat_bin, lon_bin, year)
bins_gams$group <- row.names(bins_gams)

gams_ids <- gams_all %>%
  left_join(bins_gams, by = c("lat_bin", "lon_bin", "year"))

gams_gather <- gams_ids %>%
  gather(life_stage, nObs, "caterpillars", "moths") %>%
  gather(gam, predict, "cat_predict", "moth_predict") %>%
  left_join(dplyr::select(accum_date, lat_bin, lon_bin, year, accum_wk, life_stage)) %>%
  filter(!(is.na(accum_wk)))

# GAMs phenology plots
for(yr in c(2015:2018)) {
  ggplot(dplyr::filter(gams_gather, year == yr, !is.na(nObs)), aes(x = jd_wk)) +
    geom_line(aes(y = nObs, col = life_stage), cex = 1) + 
    scale_color_manual(values=c("deepskyblue3", "skyblue1", "springgreen3", "palegreen1"), 
                       labels = c("caterpillars" = "Caterpillars",  
                                  "cat_predict" = "GAM Cats", "moths" = "Moths", "moth_predict" = "GAM Moths")) +
    scale_y_log10() +
    geom_line(aes(y = predict, col = gam), cex = 1) +
    geom_vline(aes(xintercept = accum_wk, col = life_stage), lty = 2, cex = 1, show.legend = F) +
    facet_grid(forcats::fct_reorder(factor(lat_bin), desc(lat_bin))~lon_bin) +
    scale_x_continuous(breaks = jds, labels = dates) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          strip.text = element_text(size = 20), legend.text = element_text(size = 20),
          legend.title = element_text(size = 20), axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    labs(x = "", y = "Number of observations", col = "Life stage")
  ggsave(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_gams_", yr, ".pdf"), units = "in", height = 20, width = 30)
  
  bins_yr <- bins_gams %>%
    filter(year == yr, group %in% gams_gather$group)
  
  pdf(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_pages_gams_", yr, ".pdf"), height = 5, width = 8)
  for(i in bins_yr$group) {
    df <- gams_gather %>%
      dplyr::filter(year == yr) %>%
      dplyr::filter(group == i, !is.na(nObs)) %>%
      group_by(lat_bin, lon_bin, life_stage, accum_wk) %>%
      mutate(n = sum(nObs, na.rm = T))
    nmoths <- unique(df$n)[[1]]
    ncats <- unique(df$n)[[2]]
    location <- paste0(unique(df$lat_bin), ", ", unique(df$lon_bin))
    plot <- ggplot(df, aes(x = jd_wk, y = nObs, col = life_stage)) +
      geom_line(cex = 1) + 
      scale_color_manual(values=c("deepskyblue3", "skyblue1", "springgreen3", "palegreen1"), 
                         labels = c("caterpillars" = "Caterpillars",  
                                    "cat_predict" = "GAM Cats", "moths" = "Moths", "moth_predict" = "GAM Moths")) +
      geom_line(aes(y = predict, col = gam), cex = 1) +
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
    print(plot3) # fix dimensions
  }
  dev.off()
  
}

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
ggsave("figs/inaturalist/cat_date_model_predict.pdf")

ggplot(mod_dates, aes(x = lat_bin, y = predict_diff, group = year, col = factor(year))) + geom_line(cex = 1) +
  labs(x = "Latitude", y = "Predicted difference (caterpillar date - moth date)", col = "Year")
ggsave("figs/inaturalist/diff_date_model_predict.pdf")

### Cross-correlation analysis

# write function for cross-correlations

cross_cor <- function(time_series) {
  moths <- data.frame(jd_wk = time_series$jd_wk, moths = time_series$moths)
  cats <- data.frame(jd_wk = time_series$jd_wk, cats = time_series$caterpillars)
  results <- data.frame(lag = c(0:8), r = c(NA), nobs = c(NA))
  for(lag in results$lag) {
    cats$jd_wk <- cats$jd_wk - lag*7
    lag_series <- moths %>%
      left_join(cats, by = "jd_wk") %>%
      na.omit()
    results[results$lag == lag, 2] <- cor(lag_series$moths, lag_series$cats)
    results[results$lag == lag, 3] <- nrow(lag_series)/(38-lag)
  }
  return(results)
}

inat_cross <- inat_combined %>%
  group_by(lat_bin, lon_bin, year) %>%
  filter(sum(caterpillars, na.rm = T) > 50) %>%
  group_by(lat_bin, lon_bin, year) %>%
  arrange(jd_wk) %>%
  nest() %>%
  mutate(interp = map(data, ~{
    df <- .
    int <- na.approx(df$caterpillars, maxgap = 1)
    diff <- nrow(df) - length(int)
    data.frame(jd_wk = df$jd_wk, moths = df$moths, caterpillars = c(rep(NA, diff), int))
  })) %>%
  mutate(cross_corr = map(data, ~{
    cross_cor(.)
  })) %>%
  mutate(interpolated = map(interp, ~{
    cross_cor(.)
  })) %>%
  dplyr::select(-data, -interp) %>%
  gather(method, output, cross_corr:interpolated) %>%
  unnest() 

# best lag distances for each bin

inat_lags <- inat_cross %>%
  group_by(lat_bin, lon_bin, year, method) %>%
  filter(r == max(r, na.rm = T)) %>%
  filter(nobs > 0.2)  %>%
  left_join(dplyr::select(mod_dates, lat_bin, lon_bin, year, diff), by = c("lat_bin", "lon_bin", "year"))

inat_lags_spread <- inat_lags %>%
  dplyr::select(lat_bin, lon_bin, year, method, lag) %>%
  spread(method, lag)

ggplot(inat_lags_spread, aes(x = cross_corr, y = interpolated, col = lat_bin)) +
  geom_point(size = 2, position = "jitter") + geom_abline(intercept = 0, slope = 1, lty = 2) +
  facet_wrap(~year) +
  labs(x = "Raw data", y = "Interpolated", color = "Latitude")
ggsave("figs/inaturalist/interpolated_vs_non_lags.pdf")

ggplot(inat_lags, aes(x = diff, y = lag, color = r, size = r)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(x = "Difference in 10% accum dates", y = "Best fit lag", col = "Latitude") +
  facet_wrap(~method) + theme_bw()
ggsave("figs/inaturalist/lags_vs_accum.pdf")

ggplot(inat_lags, aes(x = lat_bin, y = lag, color = r, size = r)) + geom_point() + facet_grid(method~year) + theme_bw() + 
  labs(x = "Latitude", y = "Best fit lag (weeks)", color = "r")
ggsave("figs/inaturalist/best_lags.pdf")

inat_lags_means <- inat_cross %>%
  group_by(lat_bin, lon_bin, year, method) %>%
  filter(r == max(r, na.rm = T)) %>%
  filter(nobs > 0.2) %>%
  group_by(lat_bin, year, method) %>%
  summarize(meanLag = mean(lag))

ggplot(inat_lags_means, aes(x = lat_bin, y = meanLag)) + geom_point() + facet_grid(method~year) + theme_bw() + 
  labs(x = "Latitude", y = "Mean best fit lag (weeks)", color = "Longitude")
ggsave("figs/inaturalist/mean_best_lags.pdf")

