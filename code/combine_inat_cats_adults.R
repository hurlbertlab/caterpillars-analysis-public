### Combine iNat moths and caterpillars, get species by grid cell

library(tidyverse)
library(cowplot)
library(tmap)
library(lubridate)
library(sf)
library(zoo)
library(mgcv)
library(raster)

# Read in data

inat_moths <- read.csv("data/inat_moths.csv", header = T)

inat_cats <- read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

inat_species <- read.csv("data/inat_caterpillar_species_traits.txt", sep = "\t")

cats <- inat_cats %>%
  left_join(inat_species, by = "scientific_name") %>%
  filter(family %in% c("Erebidae", "Geometridae", "Noctuidae", "Notodontidae")) %>%
  mutate(Date = as.Date(observed_on, format = "%Y-%m-%d")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  mutate(life_stage = "caterpillar")

inat_adults <- inat_moths %>%
  filter(!(id %in% inat_cats$id)) %>%
  mutate(Date = as.Date(observed_on, format = "%m/%d/%Y")) %>%
  mutate(year = year(Date), 
         jday = yday(Date)) %>%
  filter(year >= 2015) %>%
  mutate(lat_bin = 2*floor(latitude/2) + 2/2,
         lon_bin = 2*floor(longitude/2) + 2/2,
         jd_wk = 7*floor(jday/7)) %>%
  mutate(life_stage = "adult")

# Combine adult and caterpillar datasets

all_inat <- cats %>%
  dplyr::select(id, observed_on, place_guess, latitude, longitude, scientific_name,
                common_name, taxon_id, family, 
                Date, year, jday, lat_bin, lon_bin, jd_wk, life_stage) %>%
  rename(taxon_family_name = "family") %>%
  bind_rows(dplyr::select(inat_adults, -place_county_name))
#write.csv(all_inat, "data/inat_adults_cats_ENA.csv", row.names = F)

# Group obs by cell, year, life_stage, species

species_inat <- all_inat %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  filter(n_distinct(id) >= 50) %>%
  group_by(year, lat_bin, lon_bin) %>%
  filter(n_distinct(life_stage) == 2) %>%
  group_by(year, lat_bin, lon_bin, life_stage, scientific_name) %>%
  count() %>%
  filter(n >= 10, year >= 2017, lat_bin >  24)
#write.csv(species_inat, "data/inat_adults_cats_ENA_spp_counts.csv", row.names = F)

# Jaccard similarity for moths vs. caterpillars

jaccard <- function(species_list_1, species_list_2) {
  common <- length(species_list_1[species_list_1 %in% species_list_2])
  total <- length(unique(c(species_list_1, species_list_2)))
  return(common/total)
}

inat_sim <- all_inat %>%
  filter(year >= 2017, lat_bin >= 24, lat_bin <= 50) %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  filter(n_distinct(id) >= 50) %>%
  group_by(year, lat_bin, lon_bin) %>%
  filter(n_distinct(life_stage) == 2) %>%
  group_by(year, lat_bin, lon_bin) %>%
  nest() %>%
  mutate(jaccard_sim = map_dbl(data, ~{
    df <- .
    cats <- filter(df, life_stage == "caterpillar")
    moths <- filter(df, life_stage == "adult")
    cat_list <- unique(cats$scientific_name)
    moth_list <- unique(moths$scientific_name)
    jaccard(cat_list, moth_list)
  }),
  cat_sim = map_dbl(data, ~{
    df <- .
    cats <- filter(df, life_stage == "caterpillar")
    moths <- filter(df, life_stage == "adult")
    cat_list <- unique(cats$scientific_name)
    moth_list <- unique(moths$scientific_name)
    
    cat_common <- length(unique(cat_list[cat_list %in% moth_list]))
    cat_total <- length(unique(cat_list))
    
    cat_common/cat_total
    
  }))

ggplot(inat_sim, aes(x = jaccard_sim)) + 
  geom_histogram() + 
  ylim(0, 13) +
  labs(x = "Jaccard similarity between adults and caterpillar spp.", title = "iNaturalist 2-deg cells, 2017-2018")

ggplot(inat_sim, aes(x = cat_sim)) +
  geom_histogram() + 
  ylim(0, 13) +
  labs(x = "Proportion caterpillar spp in adult spp", title = "iNaturalist 2-deg cells, 2017-2018")

# Stacked bars with families for grid cells, facet

inat_families <- all_inat %>%
  filter(year >= 2017, lat_bin >= 24, lat_bin <= 50) %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  filter(n_distinct(id) >= 50) %>%
  group_by(year, lat_bin, lon_bin) %>%
  filter(n_distinct(life_stage) == 2) %>%
  group_by(year, lat_bin, lon_bin, life_stage, taxon_family_name) %>%
  count() %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  mutate(sum = sum(n), pct = n/sum)

ggplot(filter(inat_families, year == 2018), aes(x = life_stage, y = pct, fill = taxon_family_name)) +
  geom_col(position = "stack") + facet_grid(desc(lat_bin) ~ lon_bin) +
  labs(x = "", fill = "Family", y = "Proportion of observations", title = "iNaturalist 2-deg cells 2018") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Phenology curves with moth/cat species in common - % of total

inat_common <- all_inat %>%
  filter(year >= 2017, lat_bin >= 24, lat_bin <= 50) %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  filter(n_distinct(id) >= 50) %>%
  group_by(year, lat_bin, lon_bin) %>%
  filter(n_distinct(life_stage) == 2) %>%
  group_by(year, lat_bin, lon_bin) %>%
  nest() %>%
  mutate(common_spp = map(data, ~{
    df <- .
    cats <- filter(df, life_stage == "caterpillar")
    moths <- filter(df, life_stage == "adult")
    cat_list <- unique(cats$scientific_name)
    
    moth_list <- unique(moths$scientific_name)
    
    cat_common <- unique(cat_list[cat_list %in% moth_list])
    
    filter(df, scientific_name %in% cat_common)
    
  })) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  group_by(year, lat_bin, lon_bin, life_stage, jd_wk) %>%
  summarize(nObs = n_distinct(id)) %>%
  group_by(year, lat_bin, lon_bin, life_stage) %>%
  mutate(total = sum(nObs),
         accum = nObs/total)

jds = c(1, 91, 213, 335)
dates = c("Jan", "Apr", "Aug", "Dec")

ggplot(filter(inat_common, year == 2018), aes(x = jd_wk, y = accum, col = life_stage)) + 
  geom_line() + 
  geom_smooth(method = "loess", se = F, alpha = 0.5) +
  facet_grid(desc(lat_bin) ~ lon_bin) +
  scale_color_manual(values=c("deepskyblue3", "springgreen3"), 
                     labels = c("caterpillars" = "Caterpillars", 
                                "adults" = "Adults")) +
  scale_x_continuous(breaks = jds, labels = dates) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "")

bins <- inat_common %>%
  filter(year == 2018) %>%
  ungroup() %>%
  distinct(lat_bin, lon_bin) 
bins$group <- row.names(bins)

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

pdf(paste0("figs/inaturalist/phenocurves_iNat_sppCommon_pages_phenometrics_2018.pdf"), height = 5, width = 8)
for(i in bins$group) {
  df <- inat_common %>%
    dplyr::filter(year == 2018) %>%
    left_join(bins) %>%
    dplyr::filter(group == i, !is.na(nObs)) %>%
    group_by(lat_bin, lon_bin, life_stage) %>%
    mutate(n = sum(round(nObs), na.rm = T))
  nmoths <- unique(df$n)[[2]]
  ncats <- unique(df$n)[[1]]

  location <- paste0(unique(df$lat_bin), ", ", unique(df$lon_bin))
  plot <- ggplot(df, aes(x = jd_wk, y = accum, col = life_stage)) +
    geom_line(cex = 1, alpha = 0.5) + 
    scale_color_manual(values=c("deepskyblue3", "springgreen3"), 
                       labels = c("caterpillars" = "Caterpillars", 
                                  "adults" = "Adults")) +
    geom_smooth(method = "loess", se = F) +
    scale_x_continuous(breaks = jds, labels = dates, limits = c(0, 365)) +
    labs(x = "", col = "Life stage") +
    theme(legend.text = element_text(size = 15), 
          legend.title = element_text(size = 15), 
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15)) +
    ggtitle(location)
  plot2 <- plot_grid(plot, labels = c(paste0("Caterpillars = ", as.character(ncats))),
                     label_x = c(0.69), label_y = c(0.3))
  plot3 <- plot_grid(plot2, labels = c(paste0("Adults = ", as.character(nmoths))),
                     label_x = c(0.72), label_y = c(0.25))
  print(plot3) 
}
dev.off()



