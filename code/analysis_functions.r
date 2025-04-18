# Functions for working with and analyzing Caterpillars Count! data
library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
#library(sp)
#library(maptools)
#library(tidyr)
options(dplyr.summarise.inform = FALSE)

###################################
# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


# Function for calculating the mode of a series of values
# --in this particular use case, if there multiple modes, we want the largest value
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}



# Function for reading in frass data from GoogleDoc
# *if aim is to backup GoogleDoc and write to disk only, then open =F and write = T
# *if aim is to use data without writing to disk, then open = T and write = F

frassData = function(open = F, write = F) {
  require(gsheet)
  url = "https://docs.google.com/spreadsheets/d/1RwXzwhHUbP0m5gKSOVhnKZbS1C_NrbdfHLglIVCzyFc/edit#gid=1479231778"
  data = gsheet2tbl(url)
  
  if (write) {
    # Write a copy
    write.csv(data[, 1:10], paste('data/frass/frass_', Sys.Date(), '.csv', sep = ''),
              row.names = F)
  }
  if (open) { return (data) }
}



####################################
# Function for calculating and displaying arthropod phenology by week
meanDensityByWeek = function(surveyData, # merged dataframe of Survey and arthropodSighting tables for a single site
                            ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                            
                            minLength = 0,         # minimum arthropod size to include 
                            jdRange = c(1,365),
                            outlierCount = 10000,
                            plot = FALSE,
                            plotVar = 'fracSurveys', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                            minSurveyCoverage = 0.8, # minimum proportion of unique survey branches examined per week in order to include the week as a data point
                            allDates = TRUE,
                            new = TRUE,
                            color = 'black',
                            allCats = TRUE,
                            ...)                  
  
{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2]) %>%
    mutate(julianweek = 7*floor(julianday/7) + 4)
  
  effortByWeek = firstFilter %>%
    group_by(julianweek) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okWeek = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))

  if (allDates) {
    effortByWeek$okWeek = 1
  }
  
  if (!allCats) {
    secondFilter = firstFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    secondFilter = firstFilter
  }
  
  arthCount = secondFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    right_join(effortByWeek, by = 'julianweek') %>%
    filter(okWeek == 1) %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys) %>%
    arrange(julianweek) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianweek, arthCount[, plotVar], type = 'l', 
         col = color, las = 1, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  } else if (plot & new==F) {
    points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  }
  return(arthCount)
}



####################################
# Function for calculating and displaying arthropod phenology by day,
# or if surveys were split up over multiple days, then lumped by survey set
meanDensityByDay = function(surveyData, # merged dataframe of Survey and arthropodSighting tables for a single site
                             ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                             
                             minLength = 0,         # minimum arthropod size to include 
                             jdRange = c(1,365),
                             outlierCount = 10000,
                             plot = FALSE,
                             plotVar = 'fracSurveys', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                             minSurveyCoverage = 0.8, # minimum proportion of unique survey branches examined per week in order to include the week as a data point
                             allDates = TRUE,         # plot data for all dates for which any survey data exist; if FALSE, only dates where # surveys==# unique branches +/- 20%
                             new = TRUE,
                             color = 'black',
                             allCats = TRUE,
                             ...)                  

{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2])
    
  effortByDay = firstFilter %>%
    group_by(julianday) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okDay = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))
  
  if (allDates) {
    effortByDay$okDay = 1
  }

  if (!allCats) {
    secondFilter = firstFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    secondFilter = firstFilter
  }
  
  arthCount = secondFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
    group_by(julianday) %>%
    summarize(totalCount = sum(Quantity2, na.rm = T),
              numSurveysGTzero = length(unique(ID[Quantity > 0]))) %>% 
    right_join(effortByDay, by = 'julianday') %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    filter(okDay == 1) %>%
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianday, arthCount[, plotVar], type = 'l', 
         col = color, las = 1, ...)
    points(arthCount$julianday, arthCount[, plotVar], pch = 16, col = color, ...)
  } else if (plot & new==F) {
    points(arthCount$julianday, arthCount[, plotVar], type = 'l', col = color, ...)
    points(arthCount$julianday, arthCount[, plotVar], pch = 16, col = color, ...)
  }
  return(arthCount)
}





############################################
# Function for calculating summary stats about survey effort at individual sites
siteEffortSummary = function(fullDataset, 
                       year = format(Sys.Date(), "%Y"), 
                       surveyThreshold = 0.8,            # proprortion of surveys conducted to be considered a good sampling day
                       minJulianWeek = 102,              # beginning of seasonal window for tabulating # of good weeks
                       maxJulianWeek = 214)              # end of seasonal window for tabulating # of good weeks
  {
  
  summary = filter(fullDataset, Year == year) %>%
    group_by(Name, Region, cell, Latitude, Longitude, julianweek, medianGreenup) %>%
    summarize(nSurveysPerWeek = n_distinct(ID),
              nSurveyBranches = n_distinct(PlantFK)) %>%
    group_by(Name, Region, cell, Latitude, Longitude, medianGreenup) %>%
    summarize(nSurveys = sum(nSurveysPerWeek, na.rm = TRUE),
              modalSurveyBranches = Mode(nSurveyBranches),
              modalSurveyCirclesPerWeek = Mode(ceiling(nSurveysPerWeek/5)),
              medianSurveysPerWeek = median(nSurveysPerWeek, na.rm = T), 
              nWeeks = n_distinct(julianweek),
              nWeeksGoodor50Surveys = n_distinct(julianweek[(julianweek >= minJulianWeek & julianweek <= maxJulianWeek) & 
                                             (nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek | nSurveysPerWeek >= 50)]),
              nGoodWeeks = n_distinct(julianweek[julianweek >= minJulianWeek & julianweek <= maxJulianWeek & nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek]),
              medianEffortDeviation = median(abs(nSurveysPerWeek[julianweek >= minJulianWeek & julianweek <= maxJulianWeek] - 5*modalSurveyCirclesPerWeek)),
              firstDate = min(julianweek),
              lastDate = max(julianweek),
              firstGoodDate = min(julianweek[(julianweek >= minJulianWeek & julianweek <= maxJulianWeek) & 
                                               (nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek | nSurveysPerWeek >= 50)], na.rm = T),
              lastGoodDate = max(julianweek[(julianweek >= minJulianWeek & julianweek <= maxJulianWeek) & 
                                              (nSurveysPerWeek > surveyThreshold*medianSurveysPerWeek | nSurveysPerWeek >= 50)], na.rm = T),
              firstGDateAfterGreenup = firstGoodDate - medianGreenup[1],
              lastGDateAfterGreenup = lastGoodDate - medianGreenup[1])
  
  return(summary)
}




compareSamplingDatesBetweenSites = function(siteNames, year, cex.scalar = 10) {
  
  tmp = fullDataset %>%
    filter(Name %in% siteNames, Year == year) %>%
    group_by(Name, julianweek) %>%
    summarize(nSurveys = n_distinct(ID))
  
  if (nrow(tmp) == 0) {
    break("no data for those sites in that year")
  }
  
  for (i in 1:length(siteNames)) {
    
    if (i == 1) {
      
      
      par(mar = c(5, 10, 3, 1))
      plot(tmp$julianweek[tmp$Name == siteNames[i]], rep(i, sum(tmp$Name == siteNames[i])),
           xlim = range(tmp$julianweek, na.rm = T), ylim = c(0.5, length(siteNames)+1.5), 
           xlab = "Julian Week", ylab = "", yaxt = "n", 
           cex = tmp$nSurveys[tmp$Name == siteNames[i]]/cex.scalar, main = year)
      
    } else {
      
      points(tmp$julianweek[tmp$Name == siteNames[i]], rep(i, sum(tmp$Name == siteNames[i])),
             cex = tmp$nSurveys[tmp$Name == siteNames[i]]/cex.scalar)
      
    }
  }
  
  leg.pts = c(0.25, 0.5, 0.75)*(max(tmp$julianweek, na.rm = T) - min(tmp$julianweek, na.rm = T)) + min(tmp$julianweek, na.rm = T)
  leg.cex = c(0.25, 0.5, 1)*round(max(tmp$nSurveys, na.rm = T)/20)*20
  
  points(leg.pts, rep(length(siteNames) + 1, 3), cex = leg.cex/cex.scalar, pch = 16)
  
  text(leg.pts + 8, rep(length(siteNames) + 1, 3), leg.cex)
  
  mtext(c(substr(siteNames, 1, 27), "# OF SURVEYS"), 2, at = 1:(length(siteNames) + 1), las = 1, cex = .7, line = .25)
  
} 




########################################
# Criteria for inclusion (records refers to survey events)
siteSummary = function(fullDataset, year, minNumRecords = 40, minNumWeeks = 5, write = TRUE) {
  out = fullDataset %>%
    filter(Year == year) %>%
    group_by(Name, Region, Latitude, Longitude, medianGreenup, ebirdCounty) %>%
    summarize(nSurveys = n_distinct(ID),
              nDates = n_distinct(LocalDate),
              nWeeks = n_distinct(julianweek),
              nCat = sum(Group == 'caterpillar', na.rm = TRUE),
              pctCat = round(sum(Quantity[Group == 'caterpillar'] > 0)/nSurveys, 3),
              nArth = sum(Quantity, na.rm = TRUE),
              nLgArth = sum(Quantity[Length >= 10], na.rm = TRUE),
              nArthsPerSurvey = nArth/nSurveys,
              nLgArthsPerSurvey = nLgArth/nSurveys,
              pctSurvsLgArths = round(sum(Length >= 10, na.rm = TRUE)/nSurveys, 3),
              nPhoto = sum(Photo, na.rm = TRUE),
              pctPhoto = round(nPhoto/n_distinct(arthID), 3)) %>%
    arrange(desc(Latitude)) %>%
    filter(nSurveys >= minNumRecords, nWeeks >= minNumWeeks, Name != "Example Site")
  
  if (write) {
    write.table(out, paste('data/siteSummary', year, '.txt', sep = ''), sep = '\t', row.names = F)
  }
  return(out)
}



#########################################
# Function for extracting %, density, and biomass during different specified windows
#   (30-day window starting from solstice, certain window past greenup, peak period)

phenoSummary = function(fullDataset, # fullDataset format
                        postGreenupBeg = 40,     # number of days post-greenup marking the beginning of the time window
                        postGreenupEnd = 75,     # number of days post-greenup marking the end of the time window
                        fullWindowBeg = 135,     # julian day of the beginning of a specified time window (default May 15)
                        fullWindowEnd = 212,     # julian day of the end of a specified time window (default July 31)
                        minNumWeeks = 5,         # minimum number of weeks of survey data to calculate pheno summaries
                        ...) {
  
  years = unique(fullDataset$Year)
  output = data.frame(Name = NA, Year = NA, medianGreenup = NA, minJulianWeek = NA, maxJulianWeek = NA, totalSurveys = NA,
                      numGoodWeeks = NA, numWeeksPostSolsticeWindow = NA, numWeeksPostGreenupWindow = NA, 
                      pctSolstice = NA, densSolstice = NA, massSolstice= NA, pctPostGU = NA, densPostGU = NA, massPostGU = NA,
                      pctPeakDate = NA, densPeakDate = NA, massPeakDate = NA, pctPeakDateWindow = NA, densPeakDateWindow = NA,
                      massPeakDateWindow = NA, pctPeakDateGreenupWindow = NA, densPeakDateGreenupWindow = NA, 
                      massPeakDateGreenupWindow = NA, pctRollingPeakDateWindow = NA, densRollingPeakDateWindow = NA, massRollingPeakDateWindow = NA)
  
  for (y in years) {
    yearFilteredDataset = dplyr::filter(fullDataset, Year == y)
    uniqueSites = unique(yearFilteredDataset$Name)
    
    for (site in uniqueSites) {
      siteYearFilteredDataset = dplyr::filter(yearFilteredDataset, Name==site)
      
      pheno = meanDensityByWeek(siteYearFilteredDataset, allDates = FALSE, plot = FALSE, ...)
      
      if (nrow(pheno) >= minNumWeeks) {

        greenup = siteYearFilteredDataset$medianGreenup[1]
        
        siteoutput = pheno %>%
          # calculate 3-week rolling averages
          mutate(rollingPct = frollmean(fracSurveys, 3, align = "center"),
                 rollingDensity = frollmean(meanDensity, 3, align = "center"),
                 rollingBiomass = frollmean(meanBiomass, 3, align = "center")) %>%
          summarize(# mean for the month of July
            Name = site,
            Year = y,
            medianGreenup = greenup,
            minJulianWeek = min(julianweek),
            maxJulianWeek = max(julianweek),
            totalSurveys = sum(nSurveys),
            numGoodWeeks = sum(okWeek == 1),
            numWeeksPostSolsticeWindow = sum(okWeek[julianweek >= 172 & julianweek <= 202] == 1),
            numWeeksPostGreenupWindow = sum(okWeek[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)] == 1),
            pctSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                 mean(fracSurveys[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
            densSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                  mean(meanDensity[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
            massSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                  mean(meanBiomass[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
            # mean for the post-greenup window specified
            pctPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                               mean(fracSurveys[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
            densPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                                mean(meanDensity[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
            massPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                                mean(meanBiomass[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
            # peak date of the time-series unconstrained
            pctPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                 julianweek[fracSurveys == max(fracSurveys, na.rm = TRUE)][1]),
            densPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                  julianweek[meanDensity == max(meanDensity, na.rm = TRUE)][1]),
            massPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                  julianweek[meanBiomass == max(meanBiomass, na.rm = TRUE)][1]),
            # peak date within a specified, hard-coded window; [1] selects the 1st date if multiple dates have the same peak value
            pctPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                              julianweek[fracSurveys == max(fracSurveys[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
            densPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                               julianweek[meanDensity == max(meanDensity[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
            massPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                               julianweek[meanBiomass == max(meanBiomass[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
            # peak date between the beginning of the post-greenup window and the end of July; [1] selects the 1st date if multiple dates have the same peak value
            pctPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                       julianweek[fracSurveys == max(fracSurveys[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
            densPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                        julianweek[meanDensity == max(meanDensity[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
            massPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                        julianweek[meanBiomass == max(meanBiomass[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
            # peak date for the 3-week rolling average between the beginning of the post-greenup window and the end of July;
            #    -1 at the end to select the middle (rather than end) of the 3-week window
            pctRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                              julianweek[which(rollingPct == max(rollingPct[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]),
            densRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                               julianweek[which(rollingDensity == max(rollingDensity[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]),
            massRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                               julianweek[which(rollingBiomass == max(rollingBiomass[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]))
        
        output = rbind(output, siteoutput)        
      }
    } # end site
  } # end year
  out = output[-1, ]
  out[is.na(out)] = NA # converts NaN's to NA's
  return(output[-1, ])
              
}
  
  
  
  
  

#########################################
# Create a site x julianweek matrix filled with number of surveys in that site-week
siteSurveysPerWeek = function(fullDataset, 
                       year = format(Sys.Date(), "%Y"),
                       relativeToGreenup = FALSE)
{
  
  if (relativeToGreenup) {
    weekMatrix = filter(fullDataset, Year == year, Name != "Example Site") %>%
      mutate(julianweekGreenup = 7*floor((julianday - medianGreenup)/7) + 4) %>%
      distinct(Name, julianweekGreenup, ID) %>%
      count(Name, julianweekGreenup) %>%
      spread(key = julianweekGreenup, value = n)
  } else {
    weekMatrix = filter(fullDataset, Year == year, Name != "Example Site") %>%
      distinct(Name, julianweek, ID) %>%
      count(Name, julianweek) %>%
      spread(key = julianweek, value = n)
  }

  weekMatrix[is.na(weekMatrix)] = 0
  return(weekMatrix)
}



##########################################
# Plot weekly phenology for an aggregation of sites compared to the weekly
# phenology of each individual site
aggregateComponentPlot = function(dataset, ...) {
  
  meanDensityByWeek(dataset, ordersToInclude='caterpillar', plot = TRUE, new = TRUE, allDates = FALSE,
                    lwd = 4, xlab = "Julian day", ylab = "% of surveys", ...)
  
  sites = unique(dataset$Name)
  colors = rainbow(length(sites))
  i = 0
  for (s in sites) {
    i = i+1
    meanDensityByWeek(dataset[dataset$Name == s, ], ordersToInclude = 'caterpillar', 
                      plot = TRUE, new = FALSE, col = colors[i], allDates = FALSE, ...)
  }
  legend("topleft", legend = sites, lwd = 2, col = colors, bty = 'n')
  
}


######################################
# If a sitename string is too long, find the best space position for breaking into
# two separate lines. If it is not too long return NA.

breakPosition = function(string, maxCharsPerLine = 25) {
  
  if (nchar(string) <= maxCharsPerLine) {
    lineBreak = NA
  } else {
    breaks = gregexpr(" ", string)
    lineBreak = min(breaks[[1]][breaks[[1]] >= nchar(string)/2])
  }
  return(lineBreak)
}



###########################################
# Split up long site names across two lines by introducing \n in the middle at a space break

siteNameForPlotting = function(sitename, maxCharsPerLine = 25) {
  breakPos = breakPosition(sitename, maxCharsPerLine)
  
  newname = ifelse(is.na(breakPos), sitename, 
                   paste(substr(sitename, 1, breakPos - 1), "\n", 
                         substr(sitename, breakPos + 1, nchar(sitename)), sep = ""))
  return(newname)
}



###########################################
# Create multi-panel phenology plot for a set of sites

multiSitePhenoPlot = function(fullDataset, 
                              year, 
                              siteSummary, 
                              write = TRUE, 
                              monthRange = NULL, # 2-value vector with beginning and ending months for plotting;
                              # e.g., start of May - end of August would be c(5,8).
                              # If NULL, xlim will vary by site based on when surveys were conducted
                              REVI = NULL,      # 'arrivaldate' = plot window of red-eyed vireo nestlings estimated from eBird arrival date;
                                                # 'matedate1' = plot window of REVI nestlings estimated from eBird matedate1
                                                #    (date just before REVI frequency drops below .9 of max, presumably after pair formation);
                                                # 'matedate2' = plot window of REVI nestlings estimated from eBird matedate
                                                #    (date just before REVI frequency drops by .1*max for the first time);
                                                # No plotting if NULL
                              greenup = FALSE,   # add median green up date as vertical line for that location
                              filename,
                              panelRows = 4,
                              panelCols = 6,
                              col1 = 'purple3', # line color (for 1st/only line)
                              col2 = 'magenta',    # color (for 2nd line)
                              colREVI = 'plum1',
                              cex.main = 1.5,
                              cex.lab = 1,
                              cex.axis = 1,
                              cex.text = 1.5,
                              whichCatLines = 'all',  # 'all' = plot caterpillar phenology for all caterpillars,
                                                      # 'good' = plot caterpillar phenology only for 'good' caterpillars
                                                      # 'both' = plot phenologies on each panel with different colors
                              plotVar = 'fracSurveys', 
                              ordersToInclude = 'caterpillar', 
                              height = 8.5,
                              width = 11,
                              ...) {
  
  if (write) {
    pdf(paste('figs/', filename, '.pdf', sep = ''), height = height, width = width)
  }
  
  if (whichCatLines == 'all') {
    firstPlotAllCats = TRUE
    secondPlot = FALSE
  } else if (whichCatLines == 'good') {
    firstPlotAllCats = FALSE
    secondPlot = FALSE
  } else if (whichCatLines == 'both') {
    firstPlotAllCats = TRUE
    secondPlot = TRUE
  }
  
  
  # Concatenate region name to the end of site name (if it's not already there)
  siteSummary$siteNameRegion = apply(siteSummary, 1, function(x) 
    ifelse(substr(x[1], nchar(x[1])-3, nchar(x[1])) == paste(", ", x[2], sep = ""),
           x[1], paste(x[1], ", ", x[2], sep = "")))
  
  siteSummary = arrange(siteSummary, desc(Latitude))
  
  par(mfrow = c(panelRows, panelCols), mar = c(3, 2, 3, 1), oma = c(5, 5, 0, 0))
  
  counter = 0
  
  for (site in siteSummary$Name) {
    
    counter = counter + 1
    sitedata = fullDataset %>%
      filter(Name == site, Year == year)
    
    siteLabel = siteNameForPlotting(siteSummary$siteNameRegion[siteSummary$Name == site], maxCharsPerLine = 23)
    
    # goofy temporary correction for long name
    siteLabel[siteLabel == "Litzsinger Road Ecology Center\nWoodland Site A, MO"] = "Litzsinger Road Ecology\nCenter Site A, MO"
    
    
    # x-axis labels
    jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # x-axis range
    if (is.null(monthRange)) {
      # make sure xlim endpoints coincide with month labels
      if(length(unique(sitedata$julianday)) == 1) {
        minPos = which(jds == max(jds[jds <= min(sitedata$julianday)]))
        maxPos = which(jds == min(jds[jds >= max(sitedata$julianday)]))
      } else {
        minPos = max(which(jds == min(jds[jds >= min(sitedata$julianday)])) - 1, 1)    
        maxPos = min(which(jds == max(jds[jds <= max(sitedata$julianday)])) + 1, 12)
      }
    } else {
      minPos = monthRange[1]
      maxPos = monthRange[2]+1
    }
    monthLabs = minPos:(maxPos-1)
    
    # Caterpillar phenology
    caterpillarPhenology = meanDensityByWeek(sitedata, plotVar = plotVar, ordersToInclude = ordersToInclude,
                                            plot = FALSE, allDates = FALSE, ...)
    
    if (plotVar == 'fracSurveys') {
      yLabel = 'Percent of surveys'
      minY = 0
    } else if (plotVar == 'meanDensity') {
      yLabel = 'Density (# / survey)' 
      minY = min(caterpillarPhenology[, plotVar], na.rm = TRUE)
    } else if (plotVar == 'meanBiomass') {
      yLabel = 'Biomass (mg / survey)'
      #minY = min(caterpillarPhenology[, plotVar], na.rm = TRUE)
      minY = 0
    }
    maxY = max(1.3*max(caterpillarPhenology[, plotVar]), 1)
    
    # Set up plot frame
    caterpillarPhenology = meanDensityByWeek(sitedata, plotVar = plotVar,
                                            plot = TRUE, allDates = FALSE, xlab = 'Date',
                                            ylab = yLabel, lwd = 3, 
                                            xaxt = 'n', xaxs = 'i', cex.lab = cex.lab, cex.axis = cex.axis,
                                            xlim = c(jds[minPos], jds[maxPos]),
                                            ylim = c(minY, maxY), 
                                            main = siteLabel, cex.main = cex.main,
                                            allCats = firstPlotAllCats, 
                                            ordersToInclude = ordersToInclude, ...)
    
    # Plot REVI window
    if (!is.null(REVI)) {
      
      if (REVI == 'arrivaldate') {
        bird = siteSummary %>%
          filter(Name == site) %>%
          mutate(preArrival = yday(as.Date(LatestWeekWithFreq0, format = "%m/%d/%Y")) + 3, # +3 to shift from beg to middle of week
                 peakArrival = yday(as.Date(WeekOfPeakFreq, format = "%m/%d/%Y")) + 3,
                 arrival = round((preArrival + peakArrival)/2),
                 hatching = arrival + 35, # based on reproduction times from Birds of North America
                 fledging = hatching + 12) # nestling period
        rect(bird$hatching, -5, bird$fledging, 200, col = colREVI, border = NA)
      } else if (REVI == 'matedate1') {
        hatching = siteSummary$matedate1[siteSummary$Name == site] + 24 # 5d nest building + 2d pre-laying + 4d laying + 13d incubation
        if (!is.null(hatching)) {
          fledging = hatching + 12
          rect(hatching, -5, fledging, 200, col = colREVI, border = NA)
        } 
      } else if (REVI == 'matedate2') {
        hatching = siteSummary$matedate2[siteSummary$Name == site] + 24 # 5d nest building + 2d pre-laying + 4d laying + 13d incubation
        if (!is.null(hatching)) {
          fledging = hatching + 12
          rect(hatching, -5, fledging, 200, col = colREVI, border = NA)
        }
      }
      
    } # end REVI if
    
    
    
    # Month lines
    abline(v = jds, col = 'gray80')
    
    # Plot caterpillar phenology line
    caterpillarPhenology = meanDensityByWeek(sitedata, new = FALSE, plotVar = plotVar,
                                             plot = TRUE, allDates = FALSE, lwd = 3, 
                                             col = col1,  ordersToInclude = ordersToInclude,
                                             allCats = firstPlotAllCats, ...)
    
    # If plotting a second line (i.e. all vs good cats)
    if (secondPlot) {
      caterpillarPhenology2 = meanDensityByWeek(sitedata, plotVar = plotVar,
                                               plot = TRUE, allDates = FALSE, xlab = 'Date',
                                               lwd = 3, col = col2, ordersToInclude = ordersToInclude,
                                               allCats = FALSE, new = FALSE, ...)
      
    }
    
    text(jds[minPos] + 5, .9*maxY, siteSummary$nSurveys[siteSummary$Name == site],
         col = 'blue', cex = cex.text, adj = 0)
    text(jds[maxPos] - 2, .9*maxY, paste(round(siteSummary$Latitude[siteSummary$Name == site], 1), "°N", sep = ""),
         col = 'red', cex = cex.text, adj = 1)
    
    mtext(dates[monthLabs], 1, at = jds[monthLabs]+14, cex = cex.axis, line = .25)
    
    if (greenup) {
      
      if (!is.null(siteSummary$medianGreenup[siteSummary$Name == site])) {
        arrows(siteSummary$medianGreenup[siteSummary$Name == site], 0.35*(maxY - minY) + minY,
               siteSummary$medianGreenup[siteSummary$Name == site], minY, lwd = 2, col = 'limegreen', length = .15)
        
        if (counter %% (panelRows*panelCols) == 1) {
          text(siteSummary$medianGreenup[siteSummary$Name == site], 0.5*(maxY - minY) + minY, 
               'median\ngreenup', col = 'limegreen', cex = 1.5)
        }
      }
    }

    if (counter %% (panelRows*panelCols) == 0 | counter == nrow(siteSummary)) {
      mtext("Date", 1, outer = TRUE, line = 1, cex = 1.5)
      mtext(yLabel, 2, outer = TRUE, line = 1, cex = 1.5)
    }  
    
    
  } #end site
  

  if (write) {
    dev.off()
  }
}  



#####################################
# Function for adding a simple date axis and labels

jdAxis = function(jdRange, biweekly = FALSE, ...) {
  
  if (biweekly) {
    jds = c(1, 15, 32, 46, 60, 74, 91, 105, 121, 135, 152, 166, 
            182, 196, 213, 227, 244, 258, 274, 288, 305, 319, 335, 349)
    
    jd_labels = c("Jan 1", "Jan 15", "Feb 1", "Feb 15", "Mar 1", 
                  "Mar 15", "Apr 1", "Apr 15", "May 1", "May 15", 
                  "Jun 1", "Jun 15", "Jul 1", "Jul 15", "Aug 1", 
                  "Aug 15", "Sep 1", "Sep 15", "Oct 1", "Oct 15", 
                  "Nov 1", "Nov 15", "Dec 1", "Dec 15")
  } else {
    jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    jd_labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  }
  
  
  # x-axis range
  minPos = which(jds == max(jds[jds <= min(jdRange)]))
  maxPos = which(jds == min(jds[jds >= max(jdRange)]))

  axis(1, at = jds[minPos:maxPos], labels = jd_labels[minPos:maxPos], ...)
}


############################################################
# Interpolate phenology values on a daily basis for the purpose
# of color coding line segements over time

interpolatePhenoByDay = function(phenodata, var = 'fracSurveys') {
  # phenodata is object created by meanDensityByDay()
  # var can be either 'fracSurveys' or 'meanDensity'
  
  days = data.frame(julianday = min(phenodata$julianday):max(phenodata$julianday))
  
  phenodat = phenodata[, c('julianday', var)]
  names(phenodat)[2] = 'x'
  
  pheno = days %>% 
    left_join(phenodat, by = 'julianday')
  
  # Find interior NAs
  intNAs = which(sapply(1:nrow(pheno), function(row) is.na(pheno$x[row]) &
                          sum(pheno$x[1:(row-1)], na.rm = TRUE) >= 0 &
                          sum(pheno$x[(row+1):nrow(pheno)], na.rm = TRUE) >= 0))
  
  if (length(intNAs) > 0) {
    for (i in intNAs) {
      preValPos = max(which(!is.na(pheno$x[1:(i-1)])))
      postValPos = min(which(!is.na(pheno$x[(i+1):nrow(pheno)]))) + i
      
      slope = (pheno$x[postValPos] - pheno$x[preValPos])/(pheno$julianday[postValPos] - pheno$julianday[preValPos])
      
      pheno$x[i] = pheno$x[preValPos] + slope*(pheno$julianday[i] - pheno$julianday[preValPos])
    }
  }
  return(pheno)
}




# Take an interpolated pheno object as returned by interpolatePheno()
# and plot phenocurve with line rainbow-colored by date
rainbowPhenoPlot = function(phenodata, minJD = 95, maxJD = 221, ...) {
  
  colors = c('#2F2C62', '#42399B', '#4A52A7', '#59AFEA', '#7BCEB8', '#A7DA64',
             '#EFF121', '#F5952D', '#E93131', '#D70131')
  col.ramp = colorRampPalette(colors)
  cols = data.frame(julianday = minJD:maxJD, 
                    col = col.ramp(length(minJD:maxJD)))
  
  phenocol = cols %>%
    left_join(phenodata, by = 'julianday')
  phenocol$col = as.character(phenocol$col)
  
  x = phenocol$julianday
  y = phenocol$x
  
  par(bg = NA)
  plot(x, y, xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = 'n', bty = 'n')
  
  # Plot the colored line segments  
  sapply(1:(nrow(phenocol) - 1), function(jd) 
    segments(x0 = x[jd], y0 = y[jd], x1 = x[jd + 1], y1 = y[jd + 1], col = phenocol$col[jd], ...))
  
  # Plot month bar along the bottom
  
}


#####################################
# Plotting a rainbow color scale bar
rainbowScaleBar = function(minJD = 91, maxJD = 228, plot = TRUE) {
  colors = c('#2F2C62', '#42399B', '#4A52A7', '#59AFEA', '#7BCEB8', '#A7DA64',
             '#EFF121', '#F5952D', '#E93131', '#D70131')
  col.ramp = colorRampPalette(colors)
  cols = data.frame(julianday = minJD:maxJD, 
                    col = col.ramp(length(minJD:maxJD)))
  
  # labels
  monthLabels = data.frame(jd = c(1, 15, 32, 46, 60, 74, 91, 105, 121, 135, 152, 166, 
                                  182, 196, 213, 227, 244, 258, 274, 288, 305, 319, 335, 349),
                           
                           date = c("Jan 1", "Jan 15", "Feb 1", "Feb 15", "Mar 1", 
                                    "Mar 15", "Apr 1", "Apr 15", "May 1", "May 15", 
                                    "Jun 1", "Jun 15", "Jul 1", "Jul 15", "Aug 1", 
                                    "Aug 15", "Sep 1", "Sep 15", "Oct 1", "Oct 15", 
                                    "Nov 1", "Nov 15", "Dec 1", "Dec 15"))
  
  bar = left_join(cols, monthLabels, by = c('julianday' = 'jd'))
  bar$col = as.character(bar$col)
  
  barlabs = bar[!is.na(bar$date), ]
  
  if (plot) {
    png('figs/rainbow_scale.png', height = 600, width = 150, bg = NA)
    par(mar = c(0,0,0,0))
    plot(rep(1, nrow(bar)), -bar$julianday, pch = 15, cex = 4, col = bar$col,
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n', xlim = c(.9, 3.5))
    text(rep(1.4, nrow(barlabs)), -barlabs$julianday, barlabs$date, adj = 0, cex = 3)
    dev.off()
  }
}




####################################################################################
# Read eBird barchart data obtained by clicking 'Download Histogram Data' from single species line graph page

readEbirdBarchart = function(path, 
                             countyCode, 
                             speciesCode = 'reevir1', 
                             yearBeg = format(Sys.Date(), "%Y"), 
                             yearEnd= format(Sys.Date(), "%Y")) {
  filename = paste0('ebird_', countyCode, '_', speciesCode, '_', yearBeg, '_', yearEnd, '_1_12_barchart.txt')

  if (substr(path, nchar(path), nchar(path)) != "/") path = paste0(path, "/")
  fileIn = read.table(paste0(path, filename), skip = 16, header = F, sep = '\t')
  
  fileOut = data.frame(date = paste0(rep(yearEnd, 48), '-', rep(1:12, each = 4), '-', rep(c(1,8,15,22), times = 12))) %>%
    mutate(julianday = yday(date), 
           freq = unlist(fileIn[1, 2:49]),
           county = countyCode)
  return(fileOut)
}

# As above, but reading directly from 
getEbirdBarchartData = function(countyCode, speciesCode = 'reevir1', year) {
  require(data.table)
  url = paste0('https://ebird.org/barchartData?r=', countyCode, 
               '&bmo1&emo=12&byr=', year, '&eyr=', year, '&spp=', speciesCode, '&fmt=tsv')
  fileIn = fread(url, skip = 16, header = F)
  
  fileOut = data.frame(date = paste0(rep(year, 48), '-', rep(1:12, each = 4), '-', rep(c(1,8,15,22), times = 12))) %>%
    mutate(speciesCode = speciesCode,
           county = countyCode,
           Year = year,
           julianday = yday(date), 
           freq = unlist(fileIn[1, 2:49])) %>%
    select(-date)
  return(fileOut)
}


# threshold date for calculating peak bird occurrence should vary with latitude
# at 32 deg N, threshold should be 150, at 45 deg N threshold should be 210; 
# threshold = 4.615*latitude + 2.308
latitudeBasedJDthreshold = function(latitude) {
  jd = 4.615*latitude + 2.308
  return(jd)
}

# Calculates the last date within a seasonal window that varies by latitude 
# (according to the latitudeBasedJDthreshold; later window at higher latitudes)
# for which the observed bird frequency is within 0.9 (or other specified proportion)
# of the maximum frequency in that window. Thus, dips in frequency are ignored if
# frequency comes back up close to the max. This date really captures the period
# during which there is a pretty steep drop off in frequency.

matedateCalc1 = function(birdFreqDataframe, latitude, proportionOfPeak = 0.9) {
  matedate = birdFreqDataframe$julianday[birdFreqDataframe$julianday == 
                                           max(birdFreqDataframe$julianday[birdFreqDataframe$freq > proportionOfPeak*max(birdFreqDataframe$freq[birdFreqDataframe$julianday < latitudeBasedJDthreshold(latitude)]) & 
                                                                             birdFreqDataframe$julianday < latitudeBasedJDthreshold(latitude)])]
  return(matedate)
}

# Calculates the first date at which observed bird frequency drops by more than 0.1
# (or other specified proportion) times the maximum frequency. Frequency might rebound
# back up to close to the maximum value before dropping off steeply, but it is assumed that
# the first "big" dip is the period we want to characterize. Also check for a run of
# consecutive smaller dips that on their own fall below the threshold, but as a run
# exceed the threshold.

matedateCalc2 = function(birdFreqDataframe, dipFromPeak = 0.1) {
  freqDiff = diff(birdFreqDataframe$freq)
  diffRelativeToMax = freqDiff/max(birdFreqDataframe$freq, na.rm = TRUE)
  firstIndexRaw = min(which(diffRelativeToMax < -dipFromPeak))
  
  runs = rle(sign(diffRelativeToMax))
  runIDs = rep(1:length(runs$lengths), runs$lengths)
  runSum = sapply(1:length(runs$lengths), function(x) sum(diffRelativeToMax[runIDs == x]))
  runIndex = min(which(runSum < -dipFromPeak))
  runJDindex = min(which(runIDs == (runIndex)))
  
  return(birdFreqDataframe$julianday[min(firstIndexRaw, runJDindex)])
}


# Function for calculating degree of match between bird phenology and caterpillar biomass.
# Within a 3-week window centered on the middle of the projected nestling window, calculate
# average caterpillar biomass experienced. Find 3-week mean caterpillar biomass for all 
# windows +/- 3 weeks around mid-nestling window. Calculate the ratio of observed caterpillar 
# biomass to the maximum possible caterpillar biomass in the +/- 3 week windows.

# When the ratio is 1, bird phenology is as well-matched as it could be, and if it is close
# to 0 a bird could experience substantially more caterpillar biomass by shifting by up to 3
# weeks in one direction or other.

# Only calculate if there is caterpillar phenology data spanning the full range of windows.

catOverlapRatio = function(hatchingDate, 
                           caterpillarPhenology, 
                           plotVar = 'meanBiomass',
                           plusMinusWeekWindow = 2) {
  
  julianweeks = seq(4, 235, by = 7)
  
  # hatchingDate + 6 is the mid-point of the 12d nestling period
  midNestlingDate = julianweeks[abs(julianweeks - (hatchingDate + 6)) <= 3]
  
  # check that caterpillar data exist for at least 2 weeks in every 3-week window
  enoughData = c()
  for (i in 1:(2*plusMinusWeekWindow + 1)) {
    jdwindow = seq(midNestlingDate - 7*(plusMinusWeekWindow + 2 -i), midNestlingDate - 7*(plusMinusWeekWindow -i), by = 7)
    enough = length(caterpillarPhenology$julianweek[caterpillarPhenology$julianweek %in% jdwindow]) >= 2
    enoughData = c(enoughData, enough)
  }
  
  if (!FALSE %in% enoughData) {
    observedCaterpillars = mean(caterpillarPhenology[caterpillarPhenology$julianweek %in% c(midNestlingDate - 7, midNestlingDate, midNestlingDate + 7), plotVar], na.rm = T)
    
    potentialCaterpillars = vector(length = 2*plusMinusWeekWindow)
    for (i in 1:plusMinusWeekWindow) {
      
      potentialCaterpillars[2*i-1] = mean(caterpillarPhenology[caterpillarPhenology$julianweek %in% (midNestlingDate - 7*(i + -1:1)), plotVar], na.rm = T)
      potentialCaterpillars[2*i] = mean(caterpillarPhenology[caterpillarPhenology$julianweek %in% (midNestlingDate + 7*(i + -1:1)), plotVar], na.rm = T)
    }

    ratio = ifelse(max(potentialCaterpillars) > 0, observedCaterpillars/max(c(potentialCaterpillars, observedCaterpillars)), NA)

  } else {
    warning("Caterpillar data is not available for the full range of windows specified.")
    ratio = NA
  }
  return(ratio)  
}


# Functions for calculating the total GDD accumulated by a certain date,
# or the date at which a given GDD is accumulated.
#   tmean is a vector of daily average temperature values for a year,
#   which is the daily average of tmin and tmax
gddCalc = function(tmean, base = 0, asOfJD = 121) {
  
  if (length(tmean) != 365) {
    warning("tmean should include daily temperature values, but is not of length 365.")
  }
  tmean_minus_base = tmean - base
  tmean_minus_base[tmean_minus_base < 0] = 0
  
  gdd = cumsum(tmean_minus_base)
  
  gddAsOfJD = gdd[asOfJD]
  return(gddAsOfJD)
}


dateOfGDDaccumulation = function(tmean, base = 0, accumulateTo = 2400) {
  
  if (length(tmean) != 365) {
    warning("tmean should include daily temperature values, but is not of length 365.")
  }
  
  jds = 1:365
  
  tmean_minus_base = tmean - base
  tmean_minus_base[tmean_minus_base < 0] = 0
  gdd = cumsum(tmean_minus_base)
  
  dateAccumulated = min(jds[gdd >= accumulateTo])
  return(dateAccumulated)
}




######################
# Function for plotting arthropod trends over time (years)

annualTrendPlot = function(fullDataset, 
                           site, 
                           jdRange = c(121, 212), # range of julian days over which to summarize
                           plotVar = "fracSurveys",    #other option "meanDensity"
                           group = "caterpillar", 
                           minSurveysPerYear = 100,
                           showPoints = FALSE,
                           add = FALSE,
                           showTrend = TRUE,
                           yLab = NULL,
                           trend.lwd = 1,
                           trend.col = 'black',
                           trend.lty = 'solid',
                           ...) {
  
  df = filter(fullDataset, Name == site, julianday >= jdRange[1], julianday <= jdRange[2])
  
  survsPerYear = df %>%
    distinct(Year, ID) %>%
    count(Year) %>%
    rename(totalSurvs = n) 
  
  output = df %>%
    filter(Group %in% group) %>%
    group_by(Year) %>%
    summarize(nSurvs = sum(Quantity > 0, na.rm = T),
              nTot = sum(Quantity, na.rm = T)) %>%
    full_join(survsPerYear, by = 'Year') %>%
    mutate(nSurvs = ifelse(is.na(nSurvs), 0, nSurvs),
           nTot = ifelse(is.na(nTot), 0, nTot),
           fracSurveys = 100*nSurvs/totalSurvs,
           meanDensity = nTot/totalSurvs) %>%
    filter(totalSurvs >= minSurveysPerYear) %>%
    data.frame()
  
  if (nrow(output) < 3) {
    warning("Not enough data points.")
    return()
  }
  
  if (plotVar == "fracSurveys" & is.null(yLab)) { 
    yLabel = "Percent of surveys"
  } else if (plotVar == "meanDensity" & is.null(yLab)) {
    yLabel = "Mean number per survey"
  } else {
    yLabel = yLab
  }
  
  if (add) {
    points(output$Year, output[, plotVar], type = 'l', ...)
  } else {
    plot(output$Year, output[, plotVar], type = 'l', las = 1, ylab = yLabel, xlab = "", xaxt = "n", ylim = c(0, 1.2*max(output[, plotVar], na.rm = TRUE)), ...)
    axis(1, at = min(output$Year):max(output$Year), labels = round(min(output$Year):max(output$Year), 0)) # put this in bc R adds a decimal (e.g. 2019.0) to year when there are only 3 values
  }
  
  if (showPoints) { # scale points by the number of surveys conducted in each year relative to the max
    
    maxNumSurveys = max(output$totalSurvs) 
    points(output$Year, output[, plotVar], type = 'o', pch = 16, cex = 2*output$totalSurvs/maxNumSurveys, ...)
  }
  
  if (showTrend) {
    linmod = lm(output[, plotVar] ~ output$Year)
    abline(linmod, lwd = trend.lwd, col = trend.col, lty = trend.lty)
    legend("topright", legend = paste("R2 =", round(summary(linmod)$r.squared, 2)), bty = 'n')
  }
  
  return(output)
}



# Function for plotting annual trends for multiple sites in a multi-panel figure
multiSiteTrendPlot = function(fullDataset, 
                     sites,                 # vector of site names
                     jdRange = c(121, 212), # range of julian days over which to summarize
                     plotVar = "fracSurveys",    #other option "meanDensity"
                     group = "caterpillar", 
                     minSurveysPerYear = 100, # minimum number of surveys per year to be included in the regression
                     showPoints = FALSE,      # if TRUE, display data points sized by number of surveys (otherwise just line)
                     add = FALSE,
                     showTrend = TRUE,        # display the regression line and R2
                     write = FALSE,           # save the figure to a file
                     filename,                # optional filename
                     panelRows = 4,           # dimensions for multipanel plot
                     panelCols = 6,
                     col = 'purple3',         # line color for raw trend data
                     regLineCol = 'magenta',  # color for regression line
                     cex.main = 1.5,
                     cex.lab = 1,
                     cex.axis = 1,
                     cex.text = 1.5,
                     height = 8.5,
                     width = 11,
                     yLabOnPanels = FALSE,
                     ...) {

  if (write) {
    pdf(paste('figs/', filename, '.pdf', sep = ''), height = height, width = width)  
  }
  
  
  # Arrange panels in order of descending latitude
  siteTable = fullDataset %>%
    filter(Name %in% sites) %>%
    distinct(Name, Region, Latitude) %>%
    dplyr::select(Name, Region, Latitude) %>%
    arrange(desc(Latitude))
  
   # Make plot
  par(mfrow = c(panelRows, panelCols), mar = c(3, 2, 4, 1), oma = c(5, 5, 0, 0))
  
  counter = 0
  
  for (site in siteTable$Name) {
    
    
    # Add region abbreviation to the site name (if it's not already present) and then prettify
    siteTable$siteNameRegion = apply(siteTable, 1, function(x) 
      ifelse(substr(x[1], nchar(x[1])-3, nchar(x[1])) == paste(", ", x[2], sep = ""),
             x[1], paste(x[1], ", ", x[2], sep = "")))
    
    siteLabel = siteNameForPlotting(siteTable$siteNameRegion[siteTable$Name == site], maxCharsPerLine = 23)
    
    
    if (yLabOnPanels) {
      panelYLabel = NULL      # This will display the default y-axis labels inherent to annual TrendPlot
    } else {
      panelYLabel = ""        # This will suppress y-axis labels on individual panels
    }
    
    # Site trend
    trendData = annualTrendPlot(fullDataset, site, jdRange = jdRange, plotVar = plotVar, group = group, 
                                minSurveysPerYear = minSurveysPerYear, yLab = panelYLabel, col = col, ...)
    
    if (!is.null(trendData)) { #if there was enough data to calculate a trend (i.e. trendData != NULL)
    
      mtext(siteLabel, side = 3, cex = cex.main, line = 1)
      
      if (plotVar == "fracSurveys") { 
        yLabel = "Percent of surveys"
      } else if (plotVar == "meanDensity") {
        yLabel = "Mean number per survey"
      } 
      
      counter = counter + 1
      
      if (counter %% (panelRows*panelCols) == 0 | counter == length(sites)) {
        mtext("Year", 1, outer = TRUE, line = 1, cex = 1.5)
        mtext(yLabel, 2, outer = TRUE, line = 1, cex = 1.5)
      }  
      
    } # end if
  } #end site loop

  if (write) {
    dev.off()
  }

}








# Function for doing basic QA/QC check on Caterpillars Count! dataset

qaqc = function(dataToCheck,               # subset of fullDataset dataframe (i.e. new data) that requires checking
                totalArthsMax = 10,        # flag surveys where the total number of arthropods exceeds this value
                arthDiversityMax = 4,      # flag surveys where the total number of arthropod types exceeds this value
                rareArthDiversityMax = 3,  # flag surveys where the total number of rare arthropod types exceeds this value
                numberLeavesMin = 5,       # flag surveys where number of leaves is less than this value
                numberLeavesMax = 400,     # flag surveys where number of leaves falls exceeds this value
                leafLengthMax = 30,        # flag surveys where leaf length exceeds this value
                write = TRUE
                ) {
  
  survQAQC = dataToCheck %>%
    mutate(arthFlag = case_when(
      Group == 'ant' & Length > 15 | Group == 'ant' & Quantity > 50 ~ 'ant',
      Group == 'aphid' & Length > 10 | Group == 'aphid' & Quantity > 50 ~ 'aphid',
      Group == 'bee' & Length > 25 | Group == 'bee' & Quantity > 6 ~ 'bee',
      Group == 'beetle' & Length > 20 | Group == 'beetle' & Quantity > 6 ~ 'beetle',
      Group == 'caterpillar' & Length > 50 | Group == 'caterpillar' & Quantity > 6 ~ 'caterpillar',
      Group == 'daddylonglegs' & Length > 15 | Group == 'daddylonglegs' & Quantity > 6 ~ 'daddylonglegs',
      Group == 'fly' & Length > 15 | Group == 'fly' & Quantity > 6 ~ 'fly',
      Group == 'grasshopper' & Length > 20 | Group == 'grasshopper' & Quantity > 6 ~ 'grasshopper',
      Group == 'leafhopper' & Length > 20 | Group == 'leafhopper' & Quantity > 6 ~ 'leafhopper',
      Group == 'moths' & Length > 30 | Group == 'moths' & Quantity > 6 ~ 'moths',
      Group == 'other' & Length > 25 | Group == 'other' & Quantity > 6 ~ 'other',
      Group == 'spider' & Length > 15 | Group == 'spider' & Quantity > 6 ~ 'spider',
      Group == 'truebugs' & Length > 25 | Group == 'truebugs' & Quantity > 6 ~ 'truebugs',
      Group == 'unidentified' & Length > 25 | Group == 'unidentified' & Quantity > 6 ~ 'unidentified',
      TRUE ~ ''
    )) %>%
    group_by(ID, NumberOfLeaves, AverageLeafLength) %>%
    summarize(arthFlags = trimws(paste(arthFlag, collapse = " ")),
              totalArthAbund = sum(Quantity[!Group %in% c('ant', 'aphid')], na.rm = TRUE),
              totalArthDiv = n_distinct(Group[!is.na(Group)]),
              rareArthDiv = n_distinct(Group[Group %in% c('truebugs', 'grasshopper', 'daddylonglegs', 'bee', 'moths')])) %>%
    mutate(survFlag1 = ifelse(totalArthAbund > totalArthsMax, paste(arthFlags, 'totalArthAbund'), arthFlags),
           survFlag2 = ifelse(totalArthDiv > arthDiversityMax, paste(survFlag1, 'totalArthDiv'), survFlag1),
           survFlag3 = ifelse(NumberOfLeaves > numberLeavesMax | NumberOfLeaves < numberLeavesMin, paste(survFlag2, 'numLeaves'), survFlag2),
           survFlag4 = ifelse(AverageLeafLength > leafLengthMax, paste(survFlag3, 'leafLength'), survFlag3),
           flags = trimws(survFlag4),
           status = ifelse(flags == '', 'ok', 'check')) %>%
    ungroup() %>%
    dplyr::select(ID, flags, status) %>%
    right_join(dataToCheck, by = 'ID') %>%
    dplyr::select(ID, UserFKOfObserver:cell, flags, status) %>%
    mutate(actionTaken = NA)

  # Optionally write the qa/qc'ed dataset to a file
  if (write) {
    write.csv(survQAQC, paste0('dataCleaning/flagged_dataset_', Sys.Date(), '.csv'), row.names = F)
  }
  return(survQAQC)
}
