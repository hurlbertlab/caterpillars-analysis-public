## NC Moths data analysis

library(taxize)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Read in data
mnc <- read.csv("/Volumes/HurlbertLab/Databases/NC Moths/moth_records_thru2018_lineCleaned.txt", header=T, sep = ';', stringsAsFactors = F)
mnc <- read.csv("\\\\BioArk\\HurlbertLab\\Databases\\NC Moths\\moth_records_thru2018_lineCleaned.txt", header=T, sep = ';', stringsAsFactors = F)

mnc_species_complete <- read.table("data/mnc_species_complete.txt", header = T)

#### Get taxonomic information - creates mnc_species.txt, mnc_species_unid.txt, mnc_species_complete.txt #####
uniqueNames = unique(mnc$sciName)

output = data.frame(sci_name = uniqueNames, genus = NA, subfamily = NA, family = NA, superfamily = NA, ITIS_id = NA)

namecount = 1
for (name in uniqueNames) {
  print(paste(namecount, "out of", length(uniqueNames)))
  hierarchy = classification(name, db = 'itis')[[1]]
  
  # class is logical if taxonomic name does not match any existing names
  if (is.null(nrow(hierarchy))) {
    output$genus[namecount] = NA
    output$subfamily[namecount] = NA
    output$family[namecount] = NA
    output$superfamily[namecount] = NA
    output$ITIS_id[namecount] = NA
  } else if (nrow(hierarchy) == 1) {
    output$genus[namecount] = NA
    output$subfamily[namecount] = NA
    output$family[namecount] = NA
    output$superfamily[namecount] = NA
    output$ITIS_id[namecount] = NA
  } else {
    if ('genus' %in% hierarchy$rank) {
      output$genus[namecount] = hierarchy$name[hierarchy$rank == 'genus']
    } else {
      output$genus[namecount] = NA
    }
    if ('subfamily' %in% hierarchy$rank) {
      output$subfamily[namecount] = hierarchy$name[hierarchy$rank == 'subfamily']
    } else {
      output$subfamily[namecount] = NA
    }
    if ('family' %in% hierarchy$rank) {
      output$family[namecount] = hierarchy$name[hierarchy$rank == 'family']
    } else {
      output$family[namecount] = NA
    }
    if ('superfamily' %in% hierarchy$rank) {
      output$superfamily[namecount] = hierarchy$name[hierarchy$rank == 'superfamily']
    } else {
      output$superfamily[namecount] = NA
    }
    output$ITIS_id[namecount] = hierarchy$id[nrow(hierarchy)]
  }
  namecount = namecount + 1
  
} # end for n

output %>% arrange(superfamily, family, subfamily, genus, sci_name) %>%
  na.omit() %>%
  mutate(ITIS_id = as.numeric(ITIS_id)) %>%
  bind_rows(mnc_species) %>%
  distinct() %>%
  write.table('data/mnc_species.txt', sep = '\t', row.names = F)

missingSpp <- uniqueNames[!(uniqueNames %in% mnc_species$sci_name)]
write.table(missingSpp, "data/mnc_species_unid.txt", sep = '\t', row.names = F)

mnc_unid <- read.table("data/mnc_species_unid.txt", header = T, sep = "\t")
mnc_unid <- mnc_unid %>%
  select(-sci_name) %>%
  rename("sci_name" = "x")

mnc_complete <- rbind(mnc_species, mnc_unid)
write.table(mnc_complete, 'data/mnc_species_complete.txt', sep = '\t', row.names = F)

#### Phenological trends ####

mnc$jd <- yday(as.Date(mnc$date, format = "%Y-%m-%d"))
mnc$jd_wk = 7*floor(mnc$jd/7) + 4
mnc$year <- year(as.Date(mnc$date, format = "%Y-%m-%d"))

mnc_pheno <- mnc %>%
  filter(immature != T, grepl("UV", method)) %>% # only UV survey methods
  group_by(year, jd_wk) %>%
  summarize(nB = sum(as.numeric(number)), na.rm = T) %>%
  filter(year != 0)

theme_set(theme_classic())

mnc_pheno %>% group_by(year) %>% summarize(n = sum(nB, na.rm = T)) %>%
  ggplot(aes(x = year, y = n)) + geom_col() + scale_y_log10() +
  labs(y = "Number of observations", x = "Year") +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))
ggsave("figs/mnc_obsperyear.pdf", units = "in")

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

mnc_pheno %>% filter(year >= 2000) %>%
  ggplot(aes(x = jd_wk, y = nB, group = factor(year), color = factor(year))) + 
  geom_point(alpha= 0.5) + geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = jds, labels = dates) + 
  labs(y = "Number of Moths", x = "", color = "Year") +
  scale_color_viridis_d()
ggsave("figs/mnc_annualPheno.pdf", units = "in")

# Phenology of total number of individuals observed
family <- c("Geometridae", "Erebidae", "Noctuidae", "Notodontidae")
for(fam in family) {
  mnc %>%
    filter(immature != T, grepl("UV", method)) %>%
    left_join(mnc_species_complete, by = c("sciName" = "sci_name")) %>%
    group_by(year, jd_wk, family) %>%
    summarize(nB = sum(as.numeric(number)), na.rm = T) %>%
    filter(year >= 2000, family == fam) %>%
    ggplot(aes(x = jd_wk, y = nB, group = factor(year), color = factor(year))) + 
    geom_line() + 
    scale_x_continuous(breaks = jds, labels = dates) + 
    theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), 
          legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
    labs(y = "Number of Moths", x = "", color = "Year", title = fam)
  ggsave(paste0("figs/mnc_", fam, "Pheno.pdf"), units = "in")
}

# Phenology by number of records (indpendent of number of individuals)
for(fam in family) {
  mnc %>%
    filter(immature != T, grepl("UV", method)) %>%
    left_join(mnc_species_complete, by = c("sciName" = "sci_name")) %>%
    group_by(year, jd_wk, family) %>%
    summarize(nRecs = n()) %>%
    filter(year >= 2000, family == fam) %>%
    ggplot(aes(x = jd_wk, y = nRecs, group = factor(year), color = factor(year))) + 
    geom_line() + 
    scale_x_continuous(breaks = jds, labels = dates) + 
    theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), 
          legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
    labs(y = "Number of Moths", x = "", color = "Year", title = fam)
  ggsave(paste0("figs/mnc_", fam, "_nRecs_Pheno.pdf"), height = 5, width = 7, units = "in")
}

