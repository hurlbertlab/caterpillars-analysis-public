# Script for summarizing UNC Chapel Hill Campus observations
# from BIOL101.

source('code/reading_datafiles_without_users.r')

summaryYear = 2019
surveyPeriod1EndDay = 105

unc = fullDataset %>%
  filter(Name == "UNC Chapel Hill Campus", 
         Year == summaryYear,
         Quantity < 1000, # extreme rogue estimate
         UserFKOfObserver != 1584) # duplicate observer with 1930

# Random fixes
unc$Quantity[unc$Quantity == 29] = 2
unc$Quantity[unc$Quantity == 43] = 4

# Split by survey period
unc1 = filter(unc, julianday <= surveyPeriod1EndDay)
unc2 = filter(unc, julianday > surveyPeriod1EndDay)

# Plots by plant species

arthsByPlant = unc %>%
  group_by(Species) %>%
  summarize(totArths = sum(Quantity),
            numSurvs = n_distinct(ID),
            arthDens = totArths/numSurvs) %>%
  arrange(desc(arthDens))

arthsByPlant1 = unc1 %>%
  group_by(Species) %>%
  summarize(totArths = sum(Quantity),
            numSurvs = n_distinct(ID),
            arthDens = totArths/numSurvs) %>%
  arrange(desc(arthDens))

arthsByPlant2 = unc2 %>%
  group_by(Species) %>%
  summarize(totArths = sum(Quantity),
            numSurvs = n_distinct(ID),
            arthDens = totArths/numSurvs) %>%
  arrange(desc(arthDens))


par(mfrow = c(1, 1), mar = c(10, 5, 1, 1))
bplot = barplot(arthsByPlant$arthDens[1:10], ylab = "Arthropod Density", col = rainbow(10))
text(bplot, rep(-.2, 10), arthsByPlant$Species[1:10], srt = 45, xpd = 2, adj = 1)


par(mfrow = c(2, 1), mar = c(10, 5, 1, 1))
bplot = barplot(arthsByPlant1$arthDens[1:10], ylab = "Arthropod Density", col = rainbow(10))
text(bplot, rep(-1, 10), arthsByPlant1$Species[1:10], srt = 45, xpd = 2, adj = 1)
text(8, .8*max(arthsByPlant1$arthDens[1:10]), "Apr 11-15", cex = 1.5)

barplot(arthsByPlant2$arthDens[1:10], ylab = "Arthropod Density", col = rainbow(10))
text(bplot, rep(-1, 10), arthsByPlant2$Species[1:10], srt = 45, xpd = 2, adj = 1)
text(8, .8*max(arthsByPlant2$arthDens[1:10]), "Apr 16-22", cex = 1.5)

topPlantArths = filter(unc, Species == arthsByPlant$Species[1]) %>%
  count(Group) %>%
  filter(!is.na(Group)) %>%
  arrange(desc(n))

pie(topPlantArths$n, col = rainbow(6), labels = topPlantArths$Group)

# Plots by arthropod group

arthsByGroup = unc %>%
  group_by(Group) %>%
  summarize(ntot = sum(Quantity)) %>%
  arrange(desc(ntot)) %>%
  filter(!is.na(Group))

arthsByGroup1 = unc1 %>%
  group_by(Group) %>%
  summarize(ntot = sum(Quantity)) %>%
  arrange(desc(ntot)) %>%
  filter(!is.na(Group))

arthsByGroup2 = unc2 %>%
  group_by(Group) %>%
  summarize(ntot = sum(Quantity)) %>%
  arrange(desc(ntot)) %>%
  filter(!is.na(Group))


par(mfrow = c(1, 1), mar = c(8, 5, 1, 1))
bplot4 = barplot(arthsByGroup$ntot, ylab = "Total Arthropods", col = rainbow(14))
text(bplot4, rep(-5, 14), arthsByGroup$Group, srt = 45, xpd = 2, adj = 1)


par(mfrow = c(2, 1), mar = c(8, 5, 1, 1))
bplot5 = barplot(arthsByGroup1$ntot, ylab = "Total Arthropods", col = rainbow(14))
text(bplot5, rep(-5, 14), arthsByGroup1$Group, srt = 45, xpd = 2, adj = 1)
text(8, .8*max(arthsByGroup1$ntot), "Apr 11-15", cex = 1.5)

bplot6 = barplot(arthsByGroup2$ntot, ylab = "Total Arthropods", col = rainbow(14))
text(bplot6, rep(-5, 13), arthsByGroup2$Group, srt = 45, xpd = 2, adj = 1)
text(8, .8*max(arthsByGroup2$ntot), "Apr 16-22", cex = 1.5)

# Caterpillars
cats = unc %>%
  filter(Group == "caterpillar") %>%
  group_by(Species) %>%
  summarize(tot = sum(Quantity)) %>%
  arrange(desc(tot))

par(mfrow = c(1, 1), mar = c(1, 3, 1, 5))
pie(cats$tot, col = rainbow(14), labels = cats$Species)


# Comparison to 2018 class

unc18arths = fullDataset %>%
  filter(Name == "UNC Chapel Hill Campus",
         Year == 2018,
         julianday < 115) %>%
  group_by(Group) %>%
  summarize(tot = sum(Quantity)) %>% 
  filter(!is.na(Group)) %>%
  arrange(desc(tot))

par(mfrow = c(1, 1), mar = c(8, 5, 1, 1))
bplot6 = barplot(unc18arths$tot, ylab = "Total Arthropods", col = rainbow(14))
text(bplot4, rep(-5, 14), unc18arths$Group, srt = 45, xpd = 2, adj = 1)
