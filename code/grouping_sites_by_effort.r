source('code/analysis_functions.r')

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




