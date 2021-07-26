library(dplyr)
library(xml2)
library(rvest)

dataFiles = list.files('data')[grep("fullDataset", list.files('data'))]
mostRecentFile = dataFiles[length(dataFiles)]

fullDataset = read.csv(paste('data/', mostRecentFile, sep = ''), header = T)


## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/"))

github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

expert = read.csv(paste(github_raw, filter(data_links, grepl("ExpertIdentification.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)

expert2 = fullDataset %>%
  dplyr::select(Name, UserFKOfObserver, arthID, Length) %>%
  right_join(expert, by = c('arthID' = 'ArthropodSightingFK'))


overallAgreement = expert2 %>%
  filter(!OriginalGroup %in% c('other', 'unidentified')) %>%
  mutate(agree = ifelse(OriginalGroup == StandardGroup, 1, 0))

100*sum(overallAgreement$agree)/nrow(overallAgreement)


confusion = filter(overallAgreement, agree == 0) %>%
  count(OriginalGroup, StandardGroup) %>% 
  arrange(desc(n))

summary = overallAgreement %>%
  group_by(Name) %>%
  summarize(agreements = sum(agree),
            total = n(),
            pctCorrect = round(100*agreements/total, 1))

summaryLarge = overallAgreement %>%
  filter(Length == 5) %>%
  group_by(Name) %>%
  summarize(agreements = sum(agree),
            total = n(),
            pctCorrect = round(100*agreements/total, 1))

#function for calculating percent error of a given taxon
errorByTaxon <- function(taxon) {
  givenTaxon <- overallAgreement %>%
    filter(StandardGroup == taxon)
  mistakes <- givenTaxon %>%
    filter(StandardGroup != OriginalGroup)
  100* nrow(mistakes) / nrow(givenTaxon)
}

#write errorByTaxon results for each taxon to a tibble
errorForTaxa <- tibble(
  unique(overallAgreement$OriginalGroup),
  sapply(unique(overallAgreement$OriginalGroup), errorByTaxon)
  )
  names(errorForTaxa) <- (c('Taxon', 'Percent_Misidentified'))

#create bar plot for errorForTaxa
taxonErrorPlot <- barplot(errorForTaxa$Percent_Misidentified, las = 2, font = 1)

#function for calculating percent error at a given site
errorBySite <- function(site) {
  givenName <- overallAgreement %>%
    filter(Name == site)
  mistakes <- givenName %>%
    filter(StandardGroup != OriginalGroup)
  100* nrow(mistakes) / nrow(givenName)
}
#write errorBySite results for each Site to a tibble
errorForSite <- tibble(
  unique(overallAgreement$Name),
  sapply(unique(overallAgreement$Name), errorBySite)
)
names(errorForSite) <- (c('Site', 'Percent_Misidentified'))

#create bar plot for errorForSite
siteErrorPlot <- barplot(errorForSite$Percent_Misidentified, las = 2, font = 1)

#function for calculating percent error by a given user
errorByUserID <- function(UserID) {
  givenUserFKOfObserver <- overallAgreement %>%
    filter(UserFKOfObserver == UserID)
  mistakes <- givenUserFKOfObserver %>%
    filter(StandardGroup != OriginalGroup)
  100* nrow(mistakes) / nrow(givenUserFKOfObserver)
}

#write errorByUserID results for each UserID to a tibble
errorForUserID <- tibble(
  unique(overallAgreement$UserFKOfObserver),
  sapply(unique(overallAgreement$UserFKOfObserver), errorByUserID)
)
names(errorForUserID) <- (c('User_ID', 'Percent_Misidentified'))

#create bar plot for errorForUserID
siteErrorPlot <- barplot(errorForUserID$Percent_Misidentified, las = 2, font = 1)

#function for calculating percent error by a given length
errorByArthLength <- function(ArthLength) {
  givenLength <- overallAgreement %>%
    filter(Length == ArthLength)
  mistakes <- givenLength %>%
    filter(StandardGroup != OriginalGroup)
  100* nrow(mistakes) / nrow(givenLength)
}

#write errorByArthLength results for each ArthLength to a tibble
errorForArthLength <- tibble(
  unique(overallAgreement$Length),
  sapply(unique(overallAgreement$Length), errorByArthLength)
)
names(errorForArthLength) <- (c('ArthLength', 'Percent_Misidentified'))

#create bar plot for errorForArthLength
siteErrorPlot <- barplot(errorForArthLength$Percent_Misidentified, las = 2, font = 1)