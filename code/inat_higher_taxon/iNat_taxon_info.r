# Getting higher level taxonomy for iNaturalist caterpillar species

library(taxize)

# Read in data
inat = read.csv('data/inat_caterpillars_easternNA.csv', header = TRUE)

uniqueNames = unique(inat$scientific_name)

output = data.frame(scientific_name = uniqueNames, genus = NA, subfamily = NA, family = NA, superfamily = NA, ITIS_id = NA)

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

output %>% arrange(superfamily, family, subfamily, genus, scientific_name) %>%
  write.table('data/taxonomy/inat_species.txt', sep = '\t', row.names = F)

