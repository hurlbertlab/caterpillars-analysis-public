# Script for exploring biodiversity of caterpillars in CC data
library(tidyverse)
library(scatterpie)
library(maps)
library(mapdata)
library(ggrepel)
library(ggplot2)
library(patchwork)


minNumPhotosPerSite = 15

fullDataset = read.csv('data/fullDataset_2026-07-07.csv', header = T)

expert = read.csv('../caterpillars-count-data/2026-07-07_ExpertIdentification.csv')

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
  filter(sitePhotos >= minNumPhotosPerSite)

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
  left_join(sites, by = 'Name') %>%
  mutate(ShortName = substr(Name, 1, 10))




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


familyCols <- catColors %>%
  filter(family %in% familyOrder) %>%
  distinct(family, .keep_all = TRUE) %>%
  slice(match(familyOrder, family)) %>%
  pull(color)

names(familyCols) <- familyOrder

# Reorder factor levels
catFamsBySite$Family2 <- factor(catFamsBySite$Family2, levels = familyOrder)

# Rebuild pie data after factor reordering
pieDat <- catFamsBySite %>%
  select(Name, Longitude, Latitude, Family2, pct) %>%
  pivot_wider(
    names_from = Family2,
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
    color = "gray40",
    linewidth = 0.3
  ) +
  
  # Pie charts
  geom_scatterpie(
    data = pieDat2,
    aes(x = plotLon, y = plotLat),
    cols = pieCols,
    pie_scale = 0.7,
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



# Family phenology from sites in the Research Triangle
dat = cats %>% 
  filter(Name %in% c("Prairie Ridge Ecostation", 
                     "NC Botanical Garden", "Eno River State Park", 
                     "Triangle Land Conservancy - Johnston Mill Nature Preserve"), 
         Family %in% c("Geometridae", "Erebidae", "Noctuidae", 
                       "Notodontidae", "Limacodidae", "Saturniidae"))


week_prop <- dat %>%
  mutate(julianweek = as.integer(julianweek)) %>%
  group_by(julianweek, Family) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(julianweek) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Caterpillar families in MA/RI (EwA and ASRI sites)
dat2 = cats %>% 
  filter(grepl("ASRI|EwA", Name), 
         Family %in% c("Geometridae", "Erebidae", "Noctuidae", 
                       "Notodontidae", "Limacodidae", "Saturniidae"))


week_prop2 <- dat2 %>%
  mutate(julianweek = as.integer(julianweek)) %>%
  group_by(julianweek, Family) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(julianweek) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


label_df <- data.frame(
  x = c(121, 135, 152, 166, 182, 196, 213),
  label = c("May-1", "May-15", "Jun-1", "Jun-15", "Jul-1", "Jul-15", "Aug-1")
)

p1 <- ggplot(week_prop, aes(x = julianweek, y = prop, fill = Family)) +
  geom_col(color = "white", linewidth = 0.2, width = 6) +
  scale_x_continuous(limits = c(130, 214), breaks = NULL) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Relative proportion", fill = "Family") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.margin = margin(10, 10, 35, 10)
  ) +
  geom_text(
    data = label_df,
    aes(x = x, y = -0.06, label = label),
    inherit.aes = FALSE,
    size = 5
  ) +
  annotate("text", x = 156, y = 1.08, label = "North Carolina", hjust = 0, size = 8)

p2 <- ggplot(week_prop2, aes(x = julianweek, y = prop, fill = Family)) +
  geom_col(color = "white", linewidth = 0.2, width = 6) +
  scale_x_continuous(limits = c(130, 249), breaks = NULL) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Relative proportion", fill = "Family") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.margin = margin(10, 10, 35, 10)
  ) +
  geom_text(
    data = label_df2,
    aes(x = x, y = -0.06, label = label),
    inherit.aes = FALSE,
    size = 5
  ) +
  annotate("text", x = 145, y = 1.08, label = "Massachusetts / Rhode Island", hjust = 0, size = 8)

(p2 / p1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
