# Script for exploring biodiversity of caterpillars in CC data

fullDataset = read.csv('fullDataset_2026-05-13.csv', header = T)

expert = read.csv('../caterpillars-count-data/2026-05-13_ExpertIdentification.csv')

expertclass = read.csv('../caterpillars-count-data/classified_expert_identifications.csv')

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
  group_by(Family, Name) %>%
  summarize(n = n(),
            totAbund = sum(Quantity),
            nSites = n_distinct(Name),
            nPhotos = sum(Photo)) %>%
  arrange(Name, desc(n)) %>%
  filter(!is.na(Family)) %>%
  mutate(pct = 100*round(n/sum(n), 3))

sitePhotos = catFamiliesBySite %>%
  group_by(Name) %>%
  summarize(sitePhotos = sum(nPhotos)) %>%
  arrange(desc(sitePhotos)) %>%
  filter(sitePhotos >= 15)


         