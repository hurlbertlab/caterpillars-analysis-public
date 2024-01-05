# Script for evaluating which sites can be grouped together for analysis. Relies on visual inspection of sampling effort for now.

# 1) Add sites that are spatially near each other (e.g., same metro area) as list items to `sitegroups`

# 2) Make and examine grouped_sites_effort plot below (will need to expand year range and mfrow eventually)

# 3) For plots that have comparable sampling windows across multiple years, add them to the grouped_CC_sites.csv file in data folder.

# 4) Add workflow step that combines grouped sites prior to calculating phenology, etc.



source('code/analysis_functions.r')

# Read in data
latestDatafiles = list.files('data/')[grepl("fullDataset", list.files('data/'))]
fullDataset = read.csv(paste('data/', latestDatafiles[length(latestDatafiles)], sep = ''), header = T)


sitegroups = list(
  vadcsites = c('Robert C. McDonnell Campgrounds', 'Riverbend Park', "Scott's Run Nature Preserve", "Walker Nature Center", 'East Blake Lane Park', 'Broyhill Park', 'Georgetown'),
  
  acadiasites = c('Acadia NP - Alder', 'Acadia NP - Sundew'),
  
  ewasites = c('EwA at Habitat', 'EwA at the Growing Center', 'EwA at Horn Pond', 'EwA at Fresh Pond', 'EwA at the Fells'),
  
  tncasites = c('Forest Meadow (TNCA)', 'Stream Garden (TNCA)', 'Plants of Promise Garden (TNCA)', 'Azalea Repository (TNCA)'),
  
  greenvillesites = c('Inglewood area', '16 Old Paris Mtn'),
  
  riverbanks = c('Riverbanks', 'Riverbanks Zoo'),
  
  charleston = c('CawCaw Interpretive Center', 'Roxbury Park', 'Botany Bay Heritage Preserve'),
  
  merrimac = c('Merrimac Farm', 'Merrimac Farms, Nokesville, VA'),
  
  litzsinger = c('Litzsinger Road Ecology Center Woodland Site A', 'Litzsinger Road Ecology Center Woodland Site B', 'Backyard'),
  
  crosswoods = c('Crossways Survey', 'Camp Woods Survey', 'Schuylkill Center'),
  
  toronto = c('Middle Mill', 'Trinity College Caterpillar Count', 'Brookbanks Park'),
  
  lesserslave = c('Lesser Slave Lake Bird Observatory', 'Boreal Centre for Bird Conservation')
)

pdf('figs/grouped_sites_effort_2018-2021.pdf', height = 8, width = 10)
par(mfrow = c(2, 2))

for (sites in sitegroups) {

  for (y in 2018:2021) {
    
    tryCatch(
      { 
        compareSamplingDatesBetweenSites(sites, year = y)
      },
      error=function(cond) {
        plot(1,1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
        # Choose a return value in case of error
        return(NA)
      },
      warning=function(cond) {
        plot(1,1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
        # Choose a return value in case of warning
        return(NULL)
      }
    )    
  } # end year loop
  
} # end site loop

dev.off()




