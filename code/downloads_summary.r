# Script for summarizing which Caterpillars Count! materials are most downloaded

library(dplyr)
library(lubridate)
library(rvest)
library(xml2)
library(stringr)


## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/")) %>%
  distinct()

github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

downloads = read.csv(paste(github_raw, filter(data_links, grepl("Download.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Year = as.numeric(substr(Date, 1, 4)))

bypage = count(downloads, Year, Page) %>% arrange(desc(n))

byfile = count(downloads, Year, File) %>% arrange(desc(n))

yearsToInclude = 2014:year(Sys.Date())

IDdownloads = bypage %>% filter(Year %in% yearsToInclude, 
                                Page %in% c("identificationSkills", "identificationSkills/index.html")) 
sum(IDdownloads$n)

dataDownloads = byfile %>% filter(Year %in% yearsToInclude, 
                                  grepl("CSV", File))
sum(dataDownloads$n)

resourceDownloads = bypage %>% filter(Year %in% yearsToInclude, 
                                      Page %in% c("resources", "conductASurvey", "conductASurvey/index.html"))
sum(resourceDownloads$n)
