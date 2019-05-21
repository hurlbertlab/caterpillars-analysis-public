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

# Accumulation dates
yeardaycum = cats %>%
  filter(Yearday >= 138, Yearday <= 180) %>% #common seasonal window across years
  group_by(Plot, Year, Yearday) %>%
  summarize(nCats = sum(NumCaterpillars, na.rm = TRUE)) %>%
  mutate(cumcats = cumsum(nCats)) %>%
  group_by(Plot, Year) %>%
  summarize(halfCat = 0.5*max(cumcats),
            preHalfDate = max(Yearday[cumcats <= halfCat]),
            preHalfCat = max(cumcats[cumcats <= halfCat]),
            postHalfDate = min(Yearday[cumcats >= halfCat]),
            postHalfCat = min(cumcats[cumcats >= halfCat])) %>%
  mutate(halfDate = ifelse(preHalfDate == postHalfDate, preHalfDate,
                           preHalfDate + (halfCat - preHalfCat)*(postHalfDate - preHalfDate)/(postHalfCat - preHalfCat)),
         color = ifelse(Plot == 'BB', 'darkblue', 
                        ifelse(Plot == 'BS', 'red', 'lightgreen')))
  
coweetaSummary = left_join(catsbyyear, yeardaycum, by = c('Plot', 'Year'))

### Plotting 
# Average density by year
plot(catsbyyear$Year[catsbyyear$Plot == 'BB'], catsbyyear$density[catsbyyear$Plot == 'BB'], 
     type = 'l', col = 'darkblue', lwd = 3, xlab = 'Year', ylab = 'Caterpillar density')
points(catsbyyear$Year[catsbyyear$Plot == 'BS'], catsbyyear$density[catsbyyear$Plot == 'BS'], 
       type = 'l', col = 'red', lwd = 3)
points(catsbyyear$Year[catsbyyear$Plot == 'RK'], catsbyyear$density[catsbyyear$Plot == 'RK'], 
       type = 'l', col = 'lightgreen', lwd = 3)
legend("topleft", c('BB', 'BS', 'RK'), bty = 'n', lwd = 3, col = c('darkblue', 'red', 'lightgreen'))  

# Early date by year
plot(coweetaSummary$Year[coweetaSummary$Plot == 'BB'], coweetaSummary$halfDate[coweetaSummary$Plot == 'BB'], 
     type = 'l', col = 'darkblue', lwd = 3, xlab = 'Year', ylab = '50% Date')
points(coweetaSummary$Year[coweetaSummary$Plot == 'BS'], coweetaSummary$halfDate[coweetaSummary$Plot == 'BS'], 
       type = 'l', col = 'red', lwd = 3)
points(coweetaSummary$Year[coweetaSummary$Plot == 'RK'], coweetaSummary$halfDate[coweetaSummary$Plot == 'RK'], 
       type = 'l', col = 'lightgreen', lwd = 3)
legend("topleft", c('BB', 'BS', 'RK'), bty = 'n', lwd = 3, col = c('darkblue', 'red', 'lightgreen'))  

# Early date vs density
plot(coweetaSummary$halfDate, coweetaSummary$density, pch = 16, cex = 2, col = coweetaSummary$color,
     xlab = "50% caterpillar date", ylab = "Caterpillar density")


gaMoth = read.csv('//BioArk/HurlbertLab/Databases/DiscoverLifeMoths/GA_moth_data.csv', header = T)
