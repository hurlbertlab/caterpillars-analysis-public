source('code/reading_datafiles_without_users.r')

## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/"))

github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

expert = read.csv(paste(github_raw, filter(data_links, grepl("ExpertIdentification.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)



ECU = filter(fullDataset, SiteFK == 67)

NCSU = filter(fullDataset, SiteFK == 68)

# All UNC class observations occurred before the end of April
UNC = filter(fullDataset, SiteFK == 60, julianday < 120)

NCschools = rbind(ECU, NCSU, UNC)

NCphotos = filter(NCschools, Photo == 1)

expPhotos = filter(expert, ArthropodSightingFK %in% NCphotos$arthID) %>%
  filter(!OriginalGroup %in% c('other', 'unidentified')) %>%
  mutate(agree = ifelse(OriginalGroup == StandardGroup, 1, 0)) %>%
  left_join(NCschools[, c('SiteFK', 'arthID', 'Length')], by = c('ArthropodSightingFK' = 'arthID'))

confusion = filter(expPhotos, agree == 0) %>%
  mutate(incorrect = paste(OriginalGroup, StandardGroup)) %>%
  group_by(SiteFK) %>%
  count(incorrect) %>% 
  arrange(SiteFK, desc(n))

summary = expPhotos %>%
  group_by(SiteFK) %>%
  summarize(agreements = sum(agree),
            total = n(),
            pctCorrect = round(100*agreements/total, 1))

summaryLarge = expPhotos %>%
  filter(Length >= 5) %>%
  group_by(SiteFK) %>%
  summarize(agreements = sum(agree),
            total = n(),
            pctCorrect = round(100*agreements/total, 1))


overallAgreement = expert %>%
  filter(!OriginalGroup %in% c('other', 'unidentified')) %>%
  mutate(agree = ifelse(OriginalGroup == StandardGroup, 1, 0))

100*sum(overallAgreement$agree)/nrow(overallAgreement)
