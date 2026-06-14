# Script for exploring biodiversity of caterpillars in CC data
library(tidyverse)
library(scatterpie)
library(maps)
library(mapdata)
library(ggrepel)


minNumPhotosPerSite = 15

fullDataset = read.csv('data/fullDataset_2026-06-12.csv', header = T)

expert = read.csv('../caterpillars-count-data/2026-06-12_ExpertIdentification.csv')

expertclass = read.csv('../caterpillars-count-data/classified_expert_identifications.csv')

sites = unique(fullDataset[, c('Name', 'Longitude', 'Latitude', 'Region')])

beets = fullDataset %>%
  left_join(expert[, c('ArthropodSightingFK', 'Rank', 'TaxonName')], 
            by = c('arthID' = 'ArthropodSightingFK')) %>%
  filter(Group == 'beetle') %>%
  left_join(expertclass, by = c('Rank', 'TaxonName')) %>%
  rename(plantFamily = Family.x,
         Family = Family.y)

beetTaxa = beets %>%
  group_by(TaxonName, Rank, Family) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo)) %>%
  arrange(Rank, desc(n))

beetSpecies = beetTaxa %>%
  filter(Rank == "species")

topSpeciesByFam = beetSpecies %>%
  group_by(Family) %>%
  slice_head(n = 5) %>%
  arrange(Family, desc(n))

beetFamilies = beets %>%
  group_by(Family) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo),
            medianLength = median(Length)) %>%
  arrange(desc(n)) %>%
  filter(!is.na(Family)) %>%
  mutate(pct = 100*round(n/sum(n), 3))


beetFamiliesBySite = beets %>%
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

sitePhotos = beetFamiliesBySite %>%
  group_by(Name) %>%
  summarize(sitePhotos = sum(nPhotos)) %>%
  arrange(desc(sitePhotos)) %>%
  filter(sitePhotos >= minNumPhotosPerSite)

beetColors = data.frame(family = c('Curculionidae', 'Coccinellidae', 'Elateridae', 'Chrysomelidae', 'Mordellidae', 'Lampyridae', 'Cantharidae', 'other', 'unknown'),
                        commonName = c('True weevils', 'Lady beetles',
                                       'Click beetles', 'Leaf beetles',
                                       'Tumbling flower', 'Fireflies', 'Soldier beetles', 'other', 'unknown'),
                       color = pie_colors <- c(
                         "steelblue",
                         "tomato",
                         "goldenrod",
                         "seagreen3",
                         "yellow",
                         "darkorange",
                         "slategray3"
                       , 'white', 'gray90'))
beetColors$color = as.character(beetColors$color)

beetFamsBySite = beetFamiliesBySite %>%
  filter(Name %in% sitePhotos$Name) %>%
  mutate(Family2 = ifelse(!Family %in% beetColors$family, 'other', Family)) %>%
  left_join(beetColors, by = c('Family2' = 'family')) %>%
  group_by(commonName, Name, color) %>%
  summarize(pct = sum(pct)) %>%
  arrange(Name) %>%
  left_join(sites, by = 'Name') %>%
  mutate(ShortName = substr(Name, 1, 10))


# Calculate mean pct by family
familyOrder <- beetFamsBySite %>%
  group_by(commonName) %>%
  summarize(meanPct = mean(pct, na.rm = TRUE)) %>%
  
  # Put "other" last
  mutate(
    sortVal = ifelse(commonName == "other", -Inf, meanPct)
  ) %>%
  
  arrange(desc(sortVal)) %>%
  pull(commonName)

# Reorder factor levels
beetFamsBySite$commonName <- factor(beetFamsBySite$commonName, levels = familyOrder)

familyCols <- beetColors %>%
  select(commonName, color) %>%
  distinct() %>%
  filter(commonName %in% familyOrder) %>%
  mutate(commonName = factor(commonName, levels = familyOrder)) %>%
  arrange(commonName) %>%
  { setNames(.$color, as.character(.$commonName)) }


# Rebuild pie data after factor reordering
pieDat <- beetFamsBySite %>%
  select(Name, Longitude, Latitude, commonName, pct) %>%
  pivot_wider(
    names_from = commonName,
    values_from = pct,
    values_fill = 0
  ) %>%
  mutate(Name2 = substr(Name, 1, 8))

# ---------------------------
# CREATE OFFSET COORDINATES
# ---------------------------

coords <- pieDat %>%
  select(Name, Longitude, Latitude) %>%
  distinct()

# Start with original coordinates
coords2 <- coords %>%
  mutate(
    plotLon = Longitude,
    plotLat = Latitude
  )

# ---------------------------
# MASSACHUSETTS / RHODE ISLAND
# ---------------------------

ma_idx <- which(
  coords2$Longitude > -73 &
    coords2$Longitude < -69 &
    coords2$Latitude > 41 &
    coords2$Latitude < 43.5
)

coords2$plotLon[ma_idx] <-
  coords2$plotLon[ma_idx] +
  seq(-2, 2, length.out = length(ma_idx))

coords2$plotLat[ma_idx] <-
  coords2$plotLat[ma_idx] +
  rep(c(-0.8, 0.8), length.out = length(ma_idx))

# ---------------------------
# TORONTO REGION
# ---------------------------

tor_idx <- which(
  coords2$Longitude > -81 &
    coords2$Longitude < -77 &
    coords2$Latitude > 42 &
    coords2$Latitude < 45
)

coords2$plotLon[tor_idx] <-
  coords2$plotLon[tor_idx] +
  seq(-1.2, 1.2, length.out = length(tor_idx))

coords2$plotLat[tor_idx] <-
  coords2$plotLat[tor_idx] +
  rep(c(-0.8, 0.8), length.out = length(tor_idx))

# ---------------------------
# WISCONSIN
# ---------------------------

wi_idx <- which(
  coords2$Longitude > -92 &
    coords2$Longitude < -86 &
    coords2$Latitude > 42 &
    coords2$Latitude < 47
)

coords2$plotLon[wi_idx] <-
  coords2$plotLon[wi_idx] +
  seq(-2, 2, length.out = length(wi_idx))

coords2$plotLat[wi_idx] <-
  coords2$plotLat[wi_idx] +
  rep(c(-0.8, 0.8), length.out = length(wi_idx))

# ---------------------------
# NORTH CAROLINA
# ---------------------------

nc_idx <- which(
  coords2$Longitude > -80 &
    coords2$Longitude < -77 &
    coords2$Latitude > 35 &
    coords2$Latitude < 37
)

coords2$plotLon[nc_idx] <-
  coords2$plotLon[nc_idx] +
  seq(-1.5, 1.5, length.out = length(nc_idx))

coords2$plotLat[nc_idx] <-
  coords2$plotLat[nc_idx] +
  rep(c(-0.8, 0.8), length.out = length(nc_idx))

# ---------------------------
# SMALL GLOBAL JITTER
# ---------------------------

set.seed(123)

coords2 <- coords2 %>%
  mutate(
    plotLon = plotLon + runif(n(), -0.2, 0.2),
    plotLat = plotLat + runif(n(), -0.2, 0.2)
  )

# Merge displaced coordinates
pieDat2 <- pieDat %>%
  left_join(coords2,
            by = c("Name", "Longitude", "Latitude"))

# ---------------------------
# MAP DATA
# ---------------------------

world <- map_data("world")

eastNA <- world %>%
  filter(
    long > -100,
    long < -45,
    lat > 20,
    lat < 55
  )

states <- map_data("state")

canada <- map_data("world", region = "Canada")

# ---------------------------
# PIE COLUMNS
# ---------------------------

pieCols <- familyOrder

# ---------------------------
# PLOT
# ---------------------------

ggplot() +
  
  # Base map
  geom_polygon(
    data = eastNA,
    aes(long, lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  
  # State boundaries
  geom_path(
    data = states,
    aes(long, lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  # Canada outline
  geom_path(
    data = canada,
    aes(long, lat, group = group),
    color = "gray60",
    linewidth = 0.3
  ) +
  
  # Connector lines
  geom_segment(
    data = pieDat2,
    aes(
      x = Longitude,
      y = Latitude,
      xend = plotLon,
      yend = plotLat
    ),
    color = "black",
    linewidth = 0.3
  ) +
  
  # Pie charts
  geom_scatterpie(
    data = pieDat2,
    aes(x = plotLon, y = plotLat),
    cols = pieCols,
    pie_scale = 1.3,
    color = "black",
    linewidth = 0.2
  ) +
  
  # Site name labels, but kind of crowded  
  #geom_text(aes(x=plotLon, y=plotLat + 1.1, label=Name2), data=pieDat2, size=3, color="black") +
  
  coord_fixed(
    1.3,
    xlim = c(-95, -65),
    ylim = c(29, 48)
  ) +
  
  scale_fill_manual(
    values = familyCols,
    breaks = familyOrder
  ) +
  
  guides(fill = guide_legend(ncol = 1)) +
  
  theme_bw() +
  
  labs(fill = "Family")
