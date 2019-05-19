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

## CC families

cc_inat <- inat_cats %>%
  filter(user_login == "caterpillarscount") %>% # 532 obs in iNat
  left_join(inat_species, by = "scientific_name") %>%
  filter(!is.na(family))


