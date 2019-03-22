## NC Moths data analysis

library(taxize)
library(tidyverse)
library(ggplot2)

# Read in data
mnc <- read.csv("/Volumes/HurlbertLab/Databases/NC Moths/moth_records_thru2018_lineCleaned.txt", header=T, sep = '\t', stringsAsFactors = F)
mnc_species <- read.table("data/mnc_species.txt", header = T)

#### Get taxonomic information - creates mnc_species.txt #####
#uniqueNames = unique(mnc$sciName)

#output = data.frame(sci_name = uniqueNames, genus = NA, subfamily = NA, family = NA, superfamily = NA, ITIS_id = NA)

#namecount = 1
#for (name in uniqueNames) {
#  print(paste(namecount, "out of", length(uniqueNames)))
#  hierarchy = classification(name, db = 'itis')[[1]]
  
  # class is logical if taxonomic name does not match any existing names
#  if (is.null(nrow(hierarchy))) {
#    output$genus[namecount] = NA
#    output$subfamily[namecount] = NA
#    output$family[namecount] = NA
#    output$superfamily[namecount] = NA
#    output$ITIS_id[namecount] = NA
#  } else if (nrow(hierarchy) == 1) {
#    output$genus[namecount] = NA
#    output$subfamily[namecount] = NA
#    output$family[namecount] = NA
#    output$superfamily[namecount] = NA
#   output$ITIS_id[namecount] = NA
#  } else {
#    if ('genus' %in% hierarchy$rank) {
#      output$genus[namecount] = hierarchy$name[hierarchy$rank == 'genus']
#    } else {
#      output$genus[namecount] = NA
#    }
#    if ('subfamily' %in% hierarchy$rank) {
#      output$subfamily[namecount] = hierarchy$name[hierarchy$rank == 'subfamily']
#    } else {
#      output$subfamily[namecount] = NA
#    }
#    if ('family' %in% hierarchy$rank) {
#      output$family[namecount] = hierarchy$name[hierarchy$rank == 'family']
#    } else {
#      output$family[namecount] = NA
#    }
#    if ('superfamily' %in% hierarchy$rank) {
#      output$superfamily[namecount] = hierarchy$name[hierarchy$rank == 'superfamily']
#    } else {
#      output$superfamily[namecount] = NA
#    }
#    output$ITIS_id[namecount] = hierarchy$id[nrow(hierarchy)]
#  }
#  namecount = namecount + 1
  
#} # end for n

#output %>% arrange(superfamily, family, subfamily, genus, sci_name) %>%
#  write.table('data/mnc_species.txt', sep = '\t', row.names = F)


#### Phenological trends ####

mnc$jd <- yday(as.Date(mnc$date, format = "%Y-%m-%d"))
mnc$jd_wk = 7*floor(mnc$jd/7) + 4
mnc$year <- year(as.Date(mnc$date, format = "%Y-%m-%d"))

mnc_pheno <- mnc %>%
  filter(immature != T) %>%
  group_by(year, jd_wk) %>%
  summarize(nB = sum(as.numeric(number)))

theme_set(theme_classic())

mnc_pheno %>% group_by(year) %>% summarize(n = sum(nB)) %>%
  ggplot(aes(x = year, y = n)) + geom_col() + scale_y_log10() +
  labs(y = "Number of observations", x = "Year")
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

mnc %>%
  filter(immature != T) %>%
  left_join(mnc_species, by = c("sciName" = "sci_name")) %>%
  group_by(year, jd_wk, family) %>%
  summarize(nB = sum(as.numeric(number))) %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = jd_wk, y = nB, group = factor(year), color = factor(year))) + 
  geom_point(alpha= 0.5) + geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = jds, labels = dates) + 
  labs(y = "Number of Moths", x = "", color = "Year") + facet_wrap(~family, nrow = 2) + theme_bw()
ggsave("figs/mnc_familyPheno.pdf", units = "in", height = 6, width = 15)
