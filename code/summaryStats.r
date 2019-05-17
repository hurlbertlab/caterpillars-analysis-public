# Project Summary Statistics

summaryStats = function(reportYear = format(Sys.Date(), "%Y")) {
  if (!exists("fullDataset")) {
    source(paste('code/', list.files('code')[grep('CCrawdata2masterdataframe', list.files('code'))]), sep = '')
  }
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name))
  
  stats = list(
    numSurveysTotal = nrow(dataset),
    
    numSurveysThisYear = dataset %>% filter(Year == reportYear) %>% nrow(),
    
    sumSitesTotal = dataset %>% summarize(n = n_distinct(SiteFK)),
    
    sumSitesThisYear = dataset %>% filter(Year == reportYear) %>% summarize(n = n_distinct(SiteFK)),
    
    numUsers = dataset %>% summarize(n = n_distinct(UserFKOfObserver)), 
    
    numUsersThisYear = dataset %>% filter(Year == reportYear) %>% 
      summarize(n = n_distinct(UserFKOfObserver)),
    
    arthTot = dataset %>% summarize(n = sum(Quantity, na.rm = TRUE)),
    
    arthTotThisYear = dataset %>% filter(Year == reportYear) %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTot = dataset %>% filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTotThisYear = dataset %>% 
      filter(Year == reportYear, Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    medianNumSurveysPerSite = plants %>%
      count(SiteFK) %>%
      summarize(median = median(n)) %>%
      data.frame(),
    
    medianNumDatesPerSiteThisYear = dataset %>%
      filter(Year == reportYear) %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(median = median(n)) %>%
      data.frame()
  )
  return(stats)
}
