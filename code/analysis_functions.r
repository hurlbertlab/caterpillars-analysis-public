# Functions for working with and analyzing Caterpillars Count! data

# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# Function for calculating and displaying arthropod phenology
meanDensityByDay = function(surveyData, # merged dataframe of Survey and arthropodSighting tables
                            ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                            
                            minLength = 0,         # minimum arthropod size to include 
                            jdRange = c(1,365),
                            outlierCount = 10000,
                            plot = F,
                            plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                            new = T,
                            color = 'black',
                            ...)                  
  
{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2])
  
  effortByDay = firstFilter %>%
    distinct(ID, julianday) %>%
    count(julianday)
  
  arthCount = firstFilter %>%
    filter(Length >= minLength, 
           Quantity < outlierCount, 
           Group %in% ordersToInclude) %>%
    group_by(julianday) %>%
    summarize(totalCount = sum(Quantity, na.rm = T),
              numSurveysGTzero = length(unique(ID[Quantity > 0]))) %>% 
    right_join(effortByDay, by = 'julianday') %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0) %>%
    mutate(meanDensity = totalCount/n,
           fracSurveys = 100*numSurveysGTzero/n) %>%
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


