source('code/reading_datafiles_without_users.r')

## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/"))

github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

expert = read.csv(paste(github_raw, filter(data_links, grepl("ExpertIdentification.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)




overallAgreement = expert %>%
  filter(!OriginalGroup %in% c('other', 'unidentified')) %>%
  mutate(agree = ifelse(OriginalGroup == StandardGroup, 1, 0))

100*sum(overallAgreement$agree)/nrow(overallAgreement)


confusion = filter(overallAgreement, agree == 0) %>%
  count(OriginalGroup, StandardGroup) %>% 
  arrange(desc(n))

summary = overallAgreement %>%
  group_by(SiteFK) %>%
  summarize(agreements = sum(agree),
            total = n(),
            pctCorrect = round(100*agreements/total, 1))

summaryLarge = overallAgreement %>%
  filter(Length == 5) %>%
  group_by(SiteFK) %>%
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