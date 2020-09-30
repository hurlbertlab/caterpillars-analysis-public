source('code/reading_datafiles_without_users.r')

# Comparing 2019 and 2020 for sites with sufficient sampling in both
sites = c('Walker Nature Center', 'Stage Nature Center', 'Seven Islands State Birding Park', 'NC Botanical Garden', 'Ijams Nature Center', 'EwA at Fresh Pond', 'EwA at Habitat', 'EwA at the Fells')

foo = fullDataset %>% filter(Name %in% sites, julianweek <= 214, Year > 2018)

windows = foo %>% 
  distinct(Name, Year, julianweek, ID) %>% 
  count(Name, Year, julianweek) %>%
  group_by(Name, Year) %>%
  summarize(aveJD = sum(julianweek*n)/sum(n), minJD = min(julianweek, na.rm = T), maxJD = max(julianweek, na.rm = T)) %>%
  group_by(Name) %>%
  summarize(begJW = max(minJD),
            endJW = min(maxJD))

output = data.frame(Name = NA, y19 = NA, y20 = NA)
for (s in sites) {
  sitedata = foo %>% 
    filter(Name == s, 
           Year %in% 2019:2020,
           julianweek >= windows$begJW[windows$Name == s],
           julianweek <= windows$endJW[windows$Name == s])
  
  n_surveys = sitedata %>%
    group_by(Year) %>%
    summarize(n = n_distinct(ID))
  
  cats = sitedata %>%
    filter(Group == 'caterpillar') %>%
    group_by(Year) %>%
    summarize(survs_w_cat = sum(Quantity > 0, na.rm = T)) %>%
    left_join(n_surveys, by = 'Year') %>%
    mutate(catPct = 100*survs_w_cat/n)
  
  output = rbind(output, data.frame(Name = s, y19 = cats$catPct[1], y20 = cats$catPct[2]))
    
}
output = output[-1,]
