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
  
  datasetBeforeThisYear = dataset %>%
    filter(Year < reportYear)
  
  sitesThisYear = unique(datasetThisYear$SiteFK)
  
  newSitesThisYear = sitesThisYear[!sitesThisYear %in% unique(datasetBeforeThisYear$SiteFK)]
  
  stats = list(
    numSurveysTotal = dataset %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numSurveysThisYear = datasetThisYear %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numVisualSurveysTotal = filter(dataset, ObservationMethod == "Visual") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numVisualSurveysThisYear = filter(datasetThisYear, ObservationMethod == "Visual") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numBeatSurveysTotal = filter(dataset, ObservationMethod == "Beat sheet") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numBeatSurveysThisYear = filter(datasetThisYear, ObservationMethod == "Beat sheet") %>% summarize(n = n_distinct(ID)) %>% pull(n),
    
    numSitesTotal = dataset %>% summarize(n = n_distinct(SiteFK)) %>% pull(n),
    
    numSitesThisYear = datasetThisYear %>% summarize(n = n_distinct(SiteFK)) %>% pull(n),
    
    numNewSitesThisYear = length(newSitesThisYear),
    
    numRegionsTotal = dataset %>% summarize(n = n_distinct(Region)) %>% pull(n),
    
    numRegionsThisYear = datasetThisYear %>% summarize(n = n_distinct(Region)) %>% pull(n),
    
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
    
    medianNumDatesPerSitePerYear = dataset %>%
      distinct(Name, LocalDate, Year) %>%
      count(Name, Year) %>%
      group_by(Name) %>%
      summarize(meanNumDates = mean(n, na.rm = T)) %>%
      summarize(n = median(meanNumDates)) %>% 
      pull(n),
    
    medianNumDatesPerSiteThisYear = datasetThisYear %>%
      distinct(Name, LocalDate) %>%
      count(Name) %>%
      summarize(n = median(n)) %>% 
      pull(n),
    
    iNatObsTot = sum(dataset$Photo == 1, na.rm = TRUE),
    
    iNatObsThisYear = sum(datasetThisYear$Photo == 1, na.rm = TRUE),
    
    numUniversities = dataset %>% 
      distinct(Name) %>%
      filter(grepl("College", Name) | grepl("University", Name) | Name %in% c("UNC Chapel Hill Campus", "UNCW", "Georgetown", "East Campus UGA")) %>%
      summarize(n = n_distinct(Name)) %>% 
      pull(n) + 1 # adding 1 to account for UW Madison which was involved in monitoring BHRC sites

  )
  return(stats)
}



# SITE AWARDS AND OTHER STATS: For a given year, provide site summary stats and then order results by the sortingVar.
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
  
  firstYear = fullDataset %>%
    group_by(Name) %>%
    summarize(firstYear = min(Year))
  
  peakCat = fullDataset %>%
    filter(Year == reportYear) %>%
    group_by(Name, julianweek) %>%
    summarize(nSurvsPeak = n_distinct(ID),
              nCatSurvs = n_distinct(ID[Group=="caterpillar" & !is.na(Group)]), 
              pctCat = 100*nCatSurvs/nSurvsPeak) %>%
    group_by(Name) %>%
    summarize(peakPctCat = max(pctCat),
              nSurvsPeak = nSurvsPeak[pctCat == peakPctCat][1],
              peakWeek = as.Date(julianweek[pctCat == peakPctCat][1], origin = paste0(reportYear, "-01-01"))) # return the 1st if there are multiple equal peaks
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name), 
           !grepl("Coweeta", Name), Name != "Example Site",
           Year == reportYear) %>% 
    group_by(Name, Region) %>% 
    summarize(nSurvs = n_distinct(ID), 
              nDates = n_distinct(julianday), 
              nWeeks = n_distinct(julianweek), 
              nCats = sum(Quantity[Group == "caterpillar"], na.rm = T), 
              nArths = sum(Quantity, na.rm = T),
              nCatSurvs = n_distinct(ID[Group=="caterpillar" & !is.na(Group)]), 
              pctCat = 100*nCatSurvs/nSurvs, 
              nLargeAvg = sum(Quantity[Length>=10], na.rm = T)/nSurvs, 
              bigCat = ifelse(nCats > 0, max(Length[Group=="caterpillar"], na.rm = T), NA), 
              pctPhoto = 100*sum(Photo, na.rm = T)/sum(!is.na(Group)), 
              nUsers = n_distinct(UserFKOfObserver),
              maxLatitude = max(Latitude),
              minLatitude = min(Latitude),
              maxLongitude = max(Longitude),
              minLongitude = min(Longitude)) %>% 
    arrange(desc(get(sortingVar))) %>%
    left_join(firstYear, by = 'Name') %>%
    left_join(peakCat, by = 'Name')
  
  return(dataset)
}



# Function for calculating notable stats by user
annualUserStats = function(reportYear = format(Sys.Date(), "%Y"), 
                           sortingVar = 'pctCat', 
                           usernames = FALSE) {
  require(dplyr)
  
  if (!exists("fullDataset")) {
    fullDataset = read.csv(paste('data/', list.files('data')[grep('fullDataset', list.files('data'))][1], sep = ''))
  }
  
  if (!sortingVar %in% c('nSurvs', 'nDates', 'nWeeks', 'nCats', 'nCatSurvs', 'pctCat', 'nLargeAvg', 'bigCat', 'pctPhoto', 'nUsers')) {
    stop("Invalid sortingVar, which must be one of: 'nSurvs', 'nDates', 'nWeeks', 'nCats', 'nCatSurvs', 'pctCat', 'nLargeAvg', 'bigCat', 'pctPhoto', 'nUsers'")
  }
  
  firstYear = fullDataset %>%
    group_by(UserFKOfObserver) %>%
    summarize(firstYear = min(Year))
  
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name), 
           !grepl("Coweeta", Name), Name != "Example Site",
           Year == reportYear) %>% 
    group_by(UserFKOfObserver) %>% 
    summarize(nSurvs = n_distinct(ID), 
              nDates = n_distinct(julianday), 
              nWeeks = n_distinct(julianweek), 
              nCats = sum(Quantity[Group == "caterpillar"], na.rm = T), 
              nArths = sum(Quantity, na.rm = T),
              nCatSurvs = n_distinct(ID[Group=="caterpillar" & !is.na(Group)]), 
              pctCat = 100*nCatSurvs/nSurvs, 
              nLargeAvg = sum(Quantity[Length>=10], na.rm = T)/nSurvs, 
              bigCat = ifelse(nCats > 0, max(Length[Group=="caterpillar"], na.rm = T), NA),
              pctPhoto = 100*sum(Photo, na.rm = T)/sum(!is.na(Group)), 
              nSites = n_distinct(Name)) %>% 
    arrange(desc(get(sortingVar))) %>%
    left_join(firstYear, by = 'UserFKOfObserver')
  
  if(usernames) {
    users = read.csv('c:/git/caterpillars-analysis/data/2023-10-25_User.csv')
    dataset1 = left_join(dataset, users[, c('ID', 'LastName', 'FirstName')],
                         by = c('UserFKOfObserver' = 'ID')) %>%
      select(UserFKOfObserver, LastName, FirstName, nSurvs:firstYear)
  } else {
    dataset1 = dataset
  }
  
  return(dataset1)
}


    




# Project growth over time stats

# plotVar can be 
projectTrends = function(plot = F, add = F, plotVar = 'nSites', scalar = 1, ...) {
  require(dplyr)
  
  if (!exists("fullDataset")) {
    fullDataset = read.csv(paste('data/', list.files('data')[grep('fullDataset', list.files('data'))][1], sep = ''))
  }
  
  dataset = fullDataset %>%
    filter(!grepl("BBS", Name), 
           !grepl("Coweeta", Name), Name != "Example Site") 
  
  years = unique(dataset$Year)
  
  # cumulative site and user numbers
  cumulative = data.frame(Year = NULL, cumNsites = NULL, cumNusers = NULL)
  
  for (y in 1:length(years)) {
    
    cumulative = rbind(cumulative,
                   data.frame(Year = years[y],
                              cumNsites = length(unique(dataset$SiteFK[dataset$Year <= years[y]])),
                              cumNusers = length(unique(dataset$UserFKOfObserver[dataset$Year <= years[y]]))))
  }
  
  # other annual trends
  trends = dataset %>% 
    group_by(Year) %>% 
    summarize(nSurvs = n_distinct(ID), 
              nSites = n_distinct(SiteFK), 
              nCats = sum(Quantity[Group == "caterpillar"], na.rm = T), 
              nArths = sum(Quantity, na.rm = T),
              nPhotos = sum(Photo, na.rm = T),
              pctPhoto = 100*sum(Photo, na.rm = T)/sum(!is.na(Group)), 
              nUsers = n_distinct(UserFKOfObserver)) %>%
    left_join(cumulative, by = 'Year') %>%
    mutate(cumNsurvs = cumsum(nSurvs),
           cumNcats = cumsum(nCats),
           cumNarths = cumsum(nArths),
           cumNphotos = cumsum(nPhotos))
  
  if(plot) {
    
    if (!add) {
      plot(trends$Year, scalar*unlist(trends[, plotVar]), xlab = 'Year', ...)
    } else {
      points(trends$Year, scalar*unlist(trends[, plotVar]), ...)
    }
    
  }
    
  return(trends)
}

