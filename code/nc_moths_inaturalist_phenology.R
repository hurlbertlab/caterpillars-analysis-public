### Modeling phenology across multiple data sources

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)

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

# Raw correlations of phenology
for(yr in c(2015:2018)) {
  ggplot(filter(inat_combined_gather, year == yr), aes(x = jd_wk, y = nObs, col = life_stage)) +
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
    filter(year == yr)
  
  pdf(paste0("figs/inaturalist/phenocurves_iNat_mothsAndCaterpillars_pages_", yr, ".pdf"), height = 5, width = 8)
  for(i in bins_yr$group) {
    df <- inat_combined_ids %>%
      filter(year == yr) %>%
      filter(group == i) %>%
      group_by(lat_bin, lon_bin, life_stage) %>%
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

# 10% accumulation

# two models: catdate ~ mothdate*lat + year
# catdate - mothdate ~ lat + year

# Cross correlations
# funs: library(timeseries), adf.test() for stationarity, acf() timelag plot, ccf() compare two time series - which lags highest correlation
