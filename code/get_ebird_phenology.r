# Reading eBird bar chart phenology data for Red-eyed Vireo for all
# county-year combinations for which there are Caterpillars Count! data

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')

countyYears = fullDataset %>%
  distinct(Year, ebirdCounty) %>%
  filter(!is.na(ebirdCounty))
  

revi_phenology = data.frame(speciesCode = NA, county = NA, Year = NA, julianday = NA, freq = NA)
revi_pheno_summary = data.frame(speciesCode = NA, county = NA, Year = NA, matedate1 = NA, matedate2 = NA)

for (r in 1:nrow(countyYears)) {
  
  yr = countyYears$Year[r]
  cty = countyYears$ebirdCounty[r]
  tmp = getEbirdBarchartData(countyCode = cty, year = yr)
  latitude = sites2$Latitude[sites2$ebirdCounty == cty][1]

  m1 = matedateCalc1(tmp, latitude, proportionOfPeak = 0.9)
  m2 = matedateCalc2(tmp, dipFromPeak = 0.1)
  
  revi_phenology = rbind(revi_phenology, tmp)
  revi_pheno_summary = rbind(revi_pheno_summary, data.frame(speciesCode = tmp$speciesCode[1], 
                                                            county = cty, 
                                                            Year = yr, 
                                                            matedate1 = m1, 
                                                            matedate2 = m2))
}
revi_phenology = revi_phenology[-1, ]
revi_pheno_summary = revi_pheno_summary[-1, ]

write.table(revi_phenology, 'data/revi/revi_phenology_by_county_thru2019.txt', sep = '\t', row.names = F)
write.table(revi_phenology, 'data/revi/revi_pheno_summary_by_county_thru2019.txt', sep = '\t', row.names = F)
