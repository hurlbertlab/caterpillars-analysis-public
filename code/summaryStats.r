# Project Summary Statistics

summaryStats = function(reportYear = format(Sys.Date(), "%Y")) {
  if (!exists("fullDataset")) {
    #source(paste('code/', list.files('code')[grep('CCrawdata2masterdataframe', list.files('code'))], sep = ''))
    source("code/reading_datafiles_without_users.r")
  }
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name), Name != "Example Site")
  
  datasetThisYear = dataset %>%
    filter(Year == reportYear)
  
  sitesThisYear = unique(datasetThisYear$SiteFK)
  
  stats = list(
    numSurveysTotal = nrow(dataset),
    
    numSurveysThisYear = datasetThisYear %>% nrow(),
    
    numSitesTotal = dataset %>% summarize(n = n_distinct(SiteFK)),
    
    numSitesThisYear = datasetThisYear %>% summarize(n = length(sitesThisYear)),
    
    numUsers = dataset %>% summarize(n = n_distinct(UserFKOfObserver)), 
    
    numUsersThisYear = datasetThisYear %>% 
      summarize(n = n_distinct(UserFKOfObserver)),
    
    arthTot = dataset %>% summarize(n = sum(Quantity, na.rm = TRUE)),
    
    arthTotThisYear = datasetThisYear %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTot = dataset %>% filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTotThisYear = datasetThisYear %>% 
      filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    medianNumBranchesPerSite = plants %>%
      filter(Circle > 0) %>% #old branch codes that were moved or destroyed are negative
      count(SiteFK) %>%
      summarize(n = median(n)) %>%
      data.frame(),
    
    medianNumBranchesPerSiteThisYear = plants %>%
      filter(SiteFK %in% sitesThisYear, Circle > 0) %>%
      count(SiteFK) %>%
      summarize(n = median(n)) %>%
      data.frame(),
    
    medianNumSurveysPerSite = dataset %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(ID)) %>%
      summarize(n = median(totSurvs)) %>%
      data.frame(),
    
    medianNumSurveysPerSiteThisYear = datasetThisYear %>%
      group_by(SiteFK) %>%
      summarize(totSurvs = n_distinct(ID)) %>%
      summarize(n = median(totSurvs)) %>%
      data.frame(),
    
    medianNumDatesPerSite = dataset %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(n = median(n)) %>%
      data.frame(),
    
    medianNumDatesPerSiteThisYear = datasetThisYear %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(n = median(n)) %>%
      data.frame()
  )
  return(stats)
}
