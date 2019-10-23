source('code/summaryStats.r')
source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

# Read in Caterpillars Count! observations submitted to iNaturalist. Only observations from 2018 on were being
# submitted automatically, and even then some in 2018 were submitted by me to the caterpillarscount account
# inadvertently. Filter ensures we are only working with the automatically submitted data
ccinat = read.csv('data/cc_inat_data_thru_20191018.csv', header = T, quote = '\"', fill = T, stringsAsFactors = FALSE) %>%
  filter(field.caterpillars.count..observer != "", 
         taxon_order_name == 'Lepidoptera', field.butterfly.moth.life.stage == 'caterpillar')
  

catColors = data.frame(family = c('Geometridae', 'Erebidae', 'Noctuidae', 'Notodontidae', 'Limacodidae', 'Euteliidae', 'Saturniidae', 'other', 'unknown'),
                       color = c('lightgreen', 'salmon', 'skyblue', 'dodgerblue', 'orange', 'thistle', 'yellow', 'white', 'gray90'))
catColors$color = as.character(catColors$color)

catFamilies = ccinat %>% 
  count(taxon_family_name) %>% 
  mutate(totalCats = sum(n),
         totalIDcats = sum(n[taxon_family_name != ""])) %>%
  arrange(desc(n)) %>%
  mutate(family = ifelse(n < 12 & taxon_family_name != "", "other", 
                         ifelse(taxon_family_name == "", "unknown", taxon_family_name)))

catFamiliesLumped = catFamilies %>%
  group_by(family, totalCats, totalIDcats) %>%
  summarize(tot = sum(n)) %>%
  mutate(pct = round(100*tot/totalCats, 1),
         pctID = round(100*tot/totalIDcats, 1)) %>%
  left_join(catColors, by = 'family') %>%
  arrange(desc(pct))

catFamiliesLumped$family = factor(catFamiliesLumped$family, levels = as.character(catFamiliesLumped$family))

catFamiliesID = catFamiliesLumped %>%
  filter(family != "unknown")

#ggplot(catFamiliesID, aes("", pctID, fill = family)) +
#  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
#  coord_polar("y") +
#  geom_text(aes(label = paste0(round(pctID), "%")), 
#            position = position_stack(vjust = .5)) +
#  labs(x = NULL, y = NULL, fill = NULL, title = "") +
#  guides(fill = guide_legend(reverse = TRUE)) +
#  scale_fill_manual(values = catFamiliesID$color) +
#  theme_classic() +
#  theme(axis.line = element_blank(),
#        axis.text = element_blank(),
#        axis.ticks = element_blank()) + 
##  scale_y_continuous(breaks = cumsum(catFamiliesID$pctID) - catFamilies$pctID/2, labels = paste0(round(catFamilies$pctID), "%"))


siteTotalCats = ccinat %>%
  count(field.site.name, taxon_family_name) %>%
  group_by(field.site.name) %>%
  summarize(totalCats = sum(n))

siteTotalIDCats = ccinat %>%
  count(field.site.name, taxon_family_name) %>%
  filter(taxon_family_name != "") %>%
  group_by(field.site.name) %>%
  summarize(totalIDcats = sum(n))

iNatCatsByFamily = ccinat %>%
  filter(field.site.name %in% siteTotalIDCats$field.site.name[siteTotalIDCats$totalIDcats >= 5]) %>%
  left_join(catFamilies, by = 'taxon_family_name') %>%
  count(field.site.name, family) %>%
  left_join(siteTotalCats, by = 'field.site.name') %>%
  left_join(siteTotalIDCats, by = 'field.site.name') %>%
  mutate(pct = round(100*n/totalCats, 1),
         pctID = round(100*n/totalIDcats, 1)) %>%
  left_join(catColors, by = 'family')


# Overall pie chart
pie(catFamiliesID$pctID, col = catFamiliesID$color, 
    labels = paste0(catFamiliesID$family, " (", round(catFamiliesID$pctID), "%)"))

# Pie charts by site
catSiteList = unique(siteTotalIDCats$field.site.name[siteTotalIDCats$totalIDcats >= 5])

pdf('figs/caterpillar_piecharts_bysite2019_nolabels.pdf', height = 12, width = 12)
par(mfrow = c(3,3))
for (s in catSiteList) {
  sitedata = filter(iNatCatsByFamily, field.site.name == s, family != "unknown")
  pie(sitedata$pctID, col = as.character(sitedata$color), 
      labels = NA,#paste0(sitedata$family, " (", round(sitedata$pctID), "%)"),
      main = siteNameForPlotting(s))
  
}
dev.off()

# Map of mean biomass
pheno = phenoSummary(fullDataset)

pheno19 = filter(pheno, Year == 2019)

pdf('figs/easternNAmap.pdf', height = 8, width = 8)
map('world', ylim = c(25, 50), xlim = c(-95, -60), fill = TRUE, col = 'gray90')
map('state', add = TRUE)
dev.off()

