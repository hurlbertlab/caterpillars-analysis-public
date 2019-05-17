# Project Summary Statistics

summaryStats = function(reportYear = format(Sys.Date(), "%Y")) {
  source(list.files('code')[grep('reading_datafiles', list.files('code'))])
  
  stats = list(
    numSurveysTotal = nrow(fullDataset),
    
    numSurveysThisYear = fullDataset %>% filter(Year == reportYear) %>% nrow(),
    
    sumSitesTotal = fullDataset %>% summarize(n = n_distinct(SiteFK)),
    
    sumSitesThisYear = fullDataset %>% filter(Year == reportYear) %>% summarize(n = n_distinct(SiteFK)),
    
    numUsers = fullDataset %>% summarize(n = n_distinct(UserFKOfObserver)), 
    
    numUsersThisYear = fullDataset %>% filter(Year == reportYear) %>% 
      summarize(n = n_distinct(UserFKOfObserver)),
    
    arthTot = fullDataset %>% summarize(n = sum(Quantity, na.rm = TRUE)),
    
    arthTotThisYear = fullDataset %>% filter(Year == reportYear) %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTot = fullDataset %>% filter(Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    caterpillarTotThisYear = fullDataset %>% 
      filter(Year == reportYear, Group == "caterpillar") %>% 
      summarize(n = sum(Quantity, na.rm = TRUE)),
    
    medianNumSurveysPerSite = plants %>%
      count(SiteFK) %>%
      summarize(median = median(n)) %>%
      data.frame(),
    
    medianNumDatesPerSiteThisYear = fullDataset %>%
      filter(Year == reportYear) %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(median = median(n)) %>%
      data.frame()
  )
  return(stats)
}
