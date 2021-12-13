# Project Summary Statistics

summaryStats = function(reportYear = format(Sys.Date(), "%Y")) {
  require(dplyr)
  
  if (!exists("fullDataset")) {
    fullDataset = read.csv(paste('data/', list.files('data')[grep('fullDataset', list.files('data'))][1], sep = ''))
  }
  
  dataset = fullDataset %>%
    filter(#!grepl("BBS", Name), 
           !grepl("Coweeta", Name), Name != "Example Site")
  
  plants = dataset %>%
    distinct(PlantFK, SiteFK, Circle, Code, Species)
  
  datasetThisYear = dataset %>%
    filter(Year == reportYear)
  
  sitesThisYear = unique(datasetThisYear$SiteFK)
  
  stats = list(
    numSurveysTotal = dataset %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numSurveysThisYear = datasetThisYear %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numVisualSurveysTotal = filter(dataset, ObservationMethod == "Visual") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numVisualSurveysThisYear = filter(datasetThisYear, ObservationMethod == "Visual") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numBeatSurveysTotal = filter(dataset, ObservationMethod == "Beat sheet") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numBeatSurveysThisYear = filter(datasetThisYear, ObservationMethod == "Beat sheet") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numSitesTotal = dataset %>% summarize(n = n_distinct(SiteFK)) %>% pull(n),
    
    numSitesThisYear = datasetThisYear %>% summarize(n = n_distinct(SiteFK)) %>% pull(n),
    
    numUsers = dataset %>% summarize(n = n_distinct(UserFKOfObserver)) %>% pull(n), 
    
    numUsersThisYear = datasetThisYear %>% 
      summarize(n = n_distinct(UserFKOfObserver)) %>% 
      pull(n),
    
    arthTot = dataset %>% summarize(n = sum(Quantity, na.rm = TRUE)) %>% 
      pull(n),
    
    arthTotThisYear = datasetThisYear %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)) %>% 
      pull(n),
    
    caterpillarTot = dataset %>% filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)) %>% 
      pull(n),
    
    caterpillarTotThisYear = datasetThisYear %>% 
      filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)) %>% 
      pull(n),
    
    medianNumBranchesPerSite = plants %>%
      filter(Circle > 0) %>% #old branch codes that were moved or destroyed are negative
      count(SiteFK) %>%
      summarize(n = median(n)) %>% 
      pull(n),
    
    medianNumBranchesPerSiteThisYear = plants %>%
      filter(SiteFK %in% sitesThisYear, Circle > 0) %>%
      count(SiteFK) %>%
      summarize(n = median(n)) %>% 
      pull(n),
    
    medianNumBranchesSurveyedPerSite = dataset %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(Code)) %>%
      summarize(n = median(totSurvs)) %>%
      pull(n),
    
    medianNumBranchesSurveyedPerSiteThisYear = datasetThisYear %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(Code)) %>%
      summarize(n = median(totSurvs)) %>%
      pull(n),
    
    medianNumSurveysPerSite = dataset %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(ID)) %>%
      summarize(n = median(totSurvs)) %>%
      pull(n),
    
    medianNumSurveysPerSiteThisYear = datasetThisYear %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(ID)) %>%
      summarize(n = median(totSurvs)) %>% 
      pull(n),
    
    medianNumDatesPerSite = dataset %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(n = median(n)) %>% 
      pull(n),
    
    medianNumDatesPerSiteThisYear = datasetThisYear %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(n = median(n)) %>% 
      pull(n),
    
    iNatObsTot = sum(dataset$Photo == 1, na.rm = TRUE),
    
    iNatObsThisYear = sum(datasetThisYear$Photo == 1, na.rm = TRUE)

  )
  return(stats)
}



# For a given year, provide site summary stats and then order results by the sortingVar.
# Possible sortingVar values: 'nSurvs', 'nDates', 'nWeeks', 'nCats', 'nCatSurvs', 'pctCat', 
#   'nLargeAvg', 'bigCat', 'pctPhoto', 'nUsers'
annualSiteStats = function(reportYear = format(Sys.Date(), "%Y"), sortingVar = 'pctCat') {
  require(dplyr)
  
  if (!exists("fullDataset")) {
    fullDataset = read.csv(paste('data/', list.files('data')[grep('fullDataset', list.files('data'))][1], sep = ''))
  }
  
  if (!sortingVar %in% c('nSurvs', 'nDates', 'nWeeks', 'nCats', 'nCatSurvs', 'pctCat', 'nLargeAvg', 'bigCat', 'pctPhoto', 'nUsers')) {
    stop("Invalid sortingVar, which must be one of: 'nSurvs', 'nDates', 'nWeeks', 'nCats', 'nCatSurvs', 'pctCat', 'nLargeAvg', 'bigCat', 'pctPhoto', 'nUsers'")
  }
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name), 
           !grepl("Coweeta", Name), Name != "Example Site",
           Year == reportYear) %>% 
    group_by(Name) %>% 
    summarize(nSurvs = n_distinct(ID), 
              nDates = n_distinct(julianday), 
              nWeeks = n_distinct(julianweek), 
              nCats = sum(Quantity[Group == "caterpillar"]), 
              nCatSurvs = n_distinct(ID[Group=="caterpillar"]), 
              pctCat = 100*nCatSurvs/nSurvs, 
              nLargeAvg = sum(Quantity[Length>=10])/nSurvs, 
              bigCat = max(Length[Group=="caterpillar"], na.rm = T), 
              pctPhoto = 100*sum(Photo, na.rm = T)/sum(!is.na(Group)), 
              nUsers = n_distinct(UserFKOfObserver)) %>% 
    arrange(desc(get(sortingVar)))
  
  return(dataset)
}
