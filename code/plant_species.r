
library(dplyr)
library(taxize)

sites = read.csv(paste('data/', list.files('data')[grep('Site.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
plants = read.csv(paste('data/', list.files('data')[grep('Plant.csv', list.files('data'))], sep = ''), header = TRUE, stringsAsFactors = FALSE)
plantspp = data.frame(table(plants$Species))
names(plantspp) = c('plantName', 'n')
plantspp$lower = tolower(plantspp$plantName)
plantspp$sciName = NA

plants2 = left_join(plants, sites[, c('ID', 'Region')], by = c('SiteFK' = 'ID')) %>%
  distinct(Species, Region) %>% arrange(Species, Region)

multiStateSpp = plants2 %>%
  count(Species) %>%
  filter(n > 1)

states = c()
for (s in multiStateSpp$Species) {
  states = c(states, paste(plants2$Region[plants2$Species == s], collapse = ', '))
}

# Get list of regions each species is found in
plantSppRegions = plants2 %>%
  filter(! Species %in% multiStateSpp$Species) %>%
  rbind(data.frame(Species = multiStateSpp$Species, Region = states)) %>%
  arrange(Species)

for (i in 1:nrow(plantspp)) {
  hierarchy = classification(plantspp$plantName[i], db = 'itis', accepted = TRUE)[[1]]
  
  # class is logical if taxonomic name does not match any existing names
  if (!is.null(nrow(hierarchy))) {
    if ('species' %in% hierarchy$rank) {
      plantspp$sciName[i] = hierarchy$name[hierarchy$rank == 'species']
    } else if ('genus' == hierarchy$rank[nrow(hierarchy)]) {
      plantspp$sciName[i] = paste(hierarchy$name[hierarchy$rank == 'genus'], 'spp.')
    }
  }
}

plantspp2 = full_join(plantspp, plantSppRegions, by = c('plantName' = 'Species'))

write.table(plantspp2, 'z:/projects/caterpillarscount/fia by state/cc_plantlist_20190226.txt', sep = '\t', row.names = F)


allplants = left_join(fia, plantspp, by = 'lower') %>%
  select(commonName, lower, sciName) %>%
  rbind(cc_only)

write.table(allplants, 'z:/projects/caterpillarscount/fia by state/fia_plus_cc_plantlist.txt', sep = '\t', row.names = F)


#fia = read.table('z:/projects/caterpillarscount/FIA by state/tree_ids.txt', header= T, sep = '\t')
#fia$lower = tolower(fia$commonName)
