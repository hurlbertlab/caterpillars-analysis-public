# Script for exploring biodiversity of caterpillars in CC data
library(tidyverse)
library(scatterpie)
library(maps)


fullDataset = read.csv('data/fullDataset_2026-05-13.csv', header = T)

expert = read.csv('../caterpillars-count-data/2026-05-13_ExpertIdentification.csv')

expertclass = read.csv('../caterpillars-count-data/classified_expert_identifications.csv')

sites = unique(fullDataset[, c('Name', 'Longitude', 'Latitude', 'Region')])

cats = fullDataset %>%
  left_join(expert[, c('ArthropodSightingFK', 'Rank', 'TaxonName')], 
            by = c('arthID' = 'ArthropodSightingFK')) %>%
  filter(Group == 'caterpillar') %>%
  left_join(expertclass, by = c('Rank', 'TaxonName')) %>%
  rename(plantFamily = Family.x,
         Family = Family.y)

catTaxa = cats %>%
  group_by(TaxonName, Rank, Family) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo)) %>%
  arrange(Rank, desc(n))

catSpecies = catTaxa %>%
  filter(Rank == "species")

topSpeciesByFam = catSpecies %>%
  group_by(Family) %>%
  slice_head(n = 5) %>%
  arrange(Family, desc(n))

catFamilies = cats %>%
  group_by(Family) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo)) %>%
  arrange(desc(n)) %>%
  filter(!is.na(Family)) %>%
  mutate(pct = 100*round(n/sum(n), 3))
  

catFamiliesBySite = cats %>%
  filter(!is.na(Family)) %>%
  group_by(Family, Name) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo),
            .groups = "drop") %>%
  group_by(Name) %>%
  arrange(Name, desc(n)) %>%
  mutate(pct = 100 * round(n / sum(n), 3))

sitePhotos = catFamiliesBySite %>%
  group_by(Name) %>%
  summarize(sitePhotos = sum(nPhotos)) %>%
  arrange(desc(sitePhotos)) %>%
  filter(sitePhotos >= 15)

catColors = data.frame(family = c('Geometridae', 'Erebidae', 'Noctuidae', 'Notodontidae', 'Limacodidae', 'Tortricidae', 'Saturniidae', 'other', 'unknown'),
                       color = c('lightgreen', 'salmon', 'skyblue', 'dodgerblue', 'orange', 'thistle', 'yellow', 'white', 'gray90'))
catColors$color = as.character(catColors$color)

catFamsBySite = catFamiliesBySite %>%
  filter(Name %in% sitePhotos$Name) %>%
  mutate(Family2 = ifelse(!Family %in% catColors$family, 'other', Family)) %>%
  group_by(Family2, Name) %>%
  summarize(pct = sum(pct)) %>%
  left_join(catColors, by = c('Family2' = 'family')) %>%
  arrange(Name) %>%
  left_join(sites, by = 'Name')


# Calculate mean pct by family
familyOrder <- catFamsBySite %>%
  group_by(Family2) %>%
  summarize(meanPct = mean(pct, na.rm = TRUE)) %>%
  
  # Put "other" last
  mutate(
    sortVal = ifelse(Family2 == "other", -Inf, meanPct)
  ) %>%
  
  arrange(desc(sortVal)) %>%
  pull(Family2)

# Reorder factor levels
catFamsBySite$Family2 <- factor(catFamsBySite$Family2, levels = familyOrder)

# Rebuild pie data after factor reordering
pieDat <- catFamsBySite %>%
  select(Name, Longitude, Latitude, Family2, pct) %>%
  pivot_wider(
    names_from = Family2,
    values_from = pct,
    values_fill = 0
  )

# Rebuild color vector in the same order
familyCols <- catFamsBySite %>%
  distinct(Family2, color) %>%
  arrange(match(Family2, familyOrder)) %>%
  deframe()

# Pie columns
pieCols <- familyOrder

# Get map data for eastern North America
world <- map_data("world")

eastNA <- world %>%
  filter(
    long > -95,
    long < -60,
    lat > 25,
    lat < 50
  )

newEngland <- world %>%
  filter(long > -73,
         long < -70,
         lat > 42,
         lat < 43)

toronto <- world %>%
  filter(long > -79,
         long < -78,
         lat > 43.5,
         lat < 44.5)


# US states + Canadian provinces boundaries
states <- map_data("state")

canada <- map_data("world", region = "Canada")

# Plot for Eastern North America
ggplot() +
  
  geom_polygon(
    data = eastNA,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  
  geom_path(
    data = states,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_path(
    data = canada,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_scatterpie(
    data = pieDat,
    aes(x = Longitude, y = Latitude),
    cols = pieCols,
    pie_scale = 0.8,
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_fixed(
    1.3,
    xlim = c(-92, -62),
    ylim = c(28, 48)
  ) +
  
  scale_fill_manual(
    values = familyCols,
    breaks = familyOrder
  ) +
  
  guides(fill = guide_legend(ncol = 1)) +
  
  theme_bw() +
  
  labs(fill = "Family")

















pieDat <- catFamsBySite %>%
  select(Name, Longitude, Latitude, Family2, pct, color) %>%
  pivot_wider(
    names_from = Family2,
    values_from = pct,
    values_fill = 0
  )

# Create named vector of colors
familyCols <- catFamsBySite %>%
  distinct(Family2, color) %>%
  deframe()

# Get map data for eastern North America
world <- map_data("world")

eastNA <- world %>%
  filter(
    long > -95,
    long < -60,
    lat > 25,
    lat < 50
  )


# US states + Canadian provinces boundaries
states <- map_data("state")

canada <- map_data("world", region = "Canada")

# Columns containing pie values
pieCols <- setdiff(
  colnames(pieDat),
  c("Name", "Longitude", "Latitude", "color")
)

# Plot
ggplot() +
  geom_polygon(
    data = eastNA,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  # US state boundaries
  geom_path(
    data = states,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  # Canadian province boundaries
  geom_path(
    data = canada,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  # Pie charts
  geom_scatterpie(
    data = pieDat,
    aes(x = Longitude, y = Latitude),
    cols = pieCols,
    pie_scale = 0.8,
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_fixed(1.3,
              xlim = c(-94, -62),
              ylim = c(26, 49)) +
  
  scale_fill_manual(values = familyCols) +
  
  theme_bw() +
  
  labs(fill = "Family")



# Plot for Eastern North America
ggplot() +
  
  geom_polygon(
    data = eastNA,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  
  geom_path(
    data = states,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_path(
    data = canada,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_scatterpie(
    data = pieDat,
    aes(x = Longitude, y = Latitude),
    cols = pieCols,
    pie_scale = 0.8,
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_fixed(
    1.3,
    xlim = c(-92, -62),
    ylim = c(28, 48)
  ) +
  
  scale_fill_manual(
    values = familyCols,
    breaks = familyOrder
  ) +
  
  guides(fill = guide_legend(ncol = 1)) +
  
  theme_bw() +
  
  labs(fill = "Family")











# Plot for New England
ggplot() +
  
  geom_polygon(
    data = eastNA,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  
  geom_path(
    data = states,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_path(
    data = canada,
    aes(x = long, y = lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  geom_scatterpie(
    data = pieDat,
    aes(x = Longitude, y = Latitude),
    cols = pieCols,
    pie_scale = 0.07,
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_fixed(
    1.3,
    xlim = c(-72.4, -70),
    ylim = c(41.3, 42.6)
  ) +
  
  scale_fill_manual(
    values = familyCols,
    breaks = familyOrder
  ) +
  
  guides(fill = guide_legend(ncol = 1)) +
  
  theme_bw() +
  
  labs(fill = "Family")


