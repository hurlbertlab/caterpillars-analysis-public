## NC Butterflies (adult - only 47 observations of larvae) data analysis

library(taxize)
library(tidyverse)
library(ggplot2)

# Read in data
bnc <- read.csv("//BioArk/HurlbertLab/Databases/NC Butterflies/bnc_thru2018.csv", stringsAsFactors = F)
bnc_species <- read.table("data/bnc_species.txt", header = T)

#### Get taxonomic information - creates bnc_species.txt #####
uniqueNames = unique(bnc$Cname)

output = data.frame(common_name = uniqueNames, genus = NA, subfamily = NA, family = NA, superfamily = NA, ITIS_id = NA)

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

output %>% arrange(superfamily, family, subfamily, genus, common_name) %>%
  write.table('data/bnc_species.txt', sep = '\t', row.names = F)


#### Phenological trends ####

bnc$jd_wk = 7*floor(bnc$jd/7) + 4

bnc_pheno <- bnc %>%
  group_by(year, jd_wk) %>%
  summarize(nB = sum(number))

theme_set(theme_classic())

bnc %>% count(year) %>%
  ggplot(aes(x = year, y = n)) + geom_col() +
  labs(y = "Number of records", x = "Year") +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))
ggsave("figs/bnc_obsperyear.pdf", units = "in")

jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

bnc_pheno %>% filter(year >= 2000) %>%
ggplot(aes(x = jd_wk, y = nB, group = factor(year), color = factor(year))) + 
  geom_point(alpha= 0.5) + geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = jds, labels = dates) + 
  labs(y = "Number of Butterflies", x = "", color = "Year") +
  scale_color_viridis_d()
ggsave("figs/bnc_annualPheno.pdf", units = "in")

# Number of individuals
family <- c("Hesperiidae", "Nymphalidae", "Papilionidae", "Pieridae")
for(fam in family) {
  bnc %>%
    left_join(bnc_species, by = c("Cname" = "common_name")) %>%
    group_by(year, jd_wk, family) %>%
    summarize(nB = sum(number)) %>%
    filter(year >= 2000, family == fam) %>%
    ggplot(aes(x = jd_wk, y = nB, group = factor(year), color = factor(year))) + 
    geom_line() + 
    scale_x_continuous(breaks = jds, labels = dates) + 
    theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), 
          legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
    labs(y = "Number of Butterflies", x = "", color = "Year", title = fam)
  ggsave(paste0("figs/bnc_", fam, "Pheno.pdf"), units = "in")
}

# Number of unique records (independent of number)
family <- c("Hesperiidae", "Nymphalidae", "Papilionidae", "Pieridae")
for(fam in family) {
  bnc %>%
    left_join(bnc_species, by = c("Cname" = "common_name")) %>%
    group_by(year, jd_wk, family) %>%
    summarize(nB = sum(number),
              nRecs = n()) %>%
    filter(year >= 2000, family == fam) %>%
    ggplot(aes(x = jd_wk, y = nRecs, group = factor(year), color = factor(year))) + 
    geom_line() + 
    scale_x_continuous(breaks = jds, labels = dates) + 
    theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), 
          legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
    labs(y = "Number of Butterflies", x = "", color = "Year", title = fam)
  ggsave(paste0("figs/bnc_", fam, "_nRecs_Pheno.pdf"), units = "in")
}

