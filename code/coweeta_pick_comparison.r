# Summarizing Coweeta data by year

cats = read.table('z:/lab/databases/coweetacaterpillars/coweeta_cats.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE) %>%
  filter(Plot != "") %>%
  mutate(Point = trimws(Point)) %>%
  mutate(Point = gsub("\v", "", Point)) %>%
  mutate(CaterpillarFamily = trimws(CaterpillarFamily)) %>%
  mutate(CaterpillarFamily = gsub("\v", "", CaterpillarFamily),
         Comments = gsub("\v", "", Comments))

catsbyyear = cats %>%
  filter(Yearday >= 138, Yearday <= 180) %>% #common seasonal window across years
  group_by(Plot, Year) %>%
  summarize(nCats = sum(NumCaterpillars, na.rm = TRUE),
            nSurvs = n_distinct(Plot, Yearday, Point, TreeSpecies, Sample),
            density = nCats/nSurvs,
            begJD = min(Yearday),
            endJD = max(Yearday))

# Plots
plot(catsbyyear$Year[catsbyyear$Plot == 'BB'], catsbyyear$density[catsbyyear$Plot == 'BB'], 
     type = 'l', col = 'darkblue', lwd = 3, xlab = 'Year', ylab = 'Caterpillar density')
points(catsbyyear$Year[catsbyyear$Plot == 'BS'], catsbyyear$density[catsbyyear$Plot == 'BS'], 
       type = 'l', col = 'red', lwd = 3)
points(catsbyyear$Year[catsbyyear$Plot == 'RK'], catsbyyear$density[catsbyyear$Plot == 'RK'], 
       type = 'l', col = 'lightgreen', lwd = 3)
legend("topleft", c('BB', 'BS', 'RK'), bty = 'n', lwd = 3, col = c('darkblue', 'red', 'lightgreen'))  
