# Calculate "match/mismatch" between caterpillar phenology and REVI nestling period
# using the catOverlapRatio() function.

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

revi_pheno_summary = read.table('data/revi/revi_pheno_summary_by_county_thru2019.txt', header = T, sep = '\t')

phenoSum = phenoSummary(fullDataset) %>%
  left_join(sites2[, c('Name', 'ebirdCounty')], by = 'Name') %>%
  left_join(revi_pheno_summary[, c('county', 'Year', 'matedate2')], by = c('ebirdCounty' = 'county', 'Year'))

phenoSum$matchRatioBiomass = NA
phenoSum$matchRatioPct = NA

for (i in 1:nrow(phenoSum)) {
  
  if (!is.na(phenoSum$matedate2[i])) {
    tmp = filter(fullDataset, Name == phenoSum$Name[i], Year == phenoSum$Year[i]) %>% 
      meanDensityByWeek(ordersToInclude = 'caterpillar')
    
    # 5d nest building + 2d pre-laying + 4d laying + 13d incubation = 24 d
    phenoSum$matchRatioBiomass[i] = catOverlapRatio(phenoSum$matedate2[i] + 24, tmp, 
                                                    plotVar = 'meanBiomass', plusMinusWeekWindow = 2)
    phenoSum$matchRatioPct[i] = catOverlapRatio(phenoSum$matedate2[i] + 24, tmp, 
                                                    plotVar = 'fracSurveys', plusMinusWeekWindow = 2)
    
    rm(tmp)
    
  } else {
    phenoSum$matchRatioBiomass[i] = NA
    phenoSum$matchRatioPct[i] = NA
  }
}

match = filter(phenoSum, !is.na(matchRatioBiomass))            



