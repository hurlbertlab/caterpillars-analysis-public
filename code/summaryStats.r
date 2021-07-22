# Project Summary Statistics

summaryStats = function(reportYear = format(Sys.Date(), "%Y")) {
  require(dplyr)
  
  if (!exists("fullDataset")) {
    #source(paste('code/', list.files('code')[grep('CCrawdata2masterdataframe', list.files('code'))], sep = ''))
    source("code/reading_datafiles_without_users.r")
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
