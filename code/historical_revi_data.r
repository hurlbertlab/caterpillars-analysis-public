# Getting REVI phenology data from Nipissing, Ontario for comparison
# to historical data in Lawrence 1953

source('code/analysis_functions.r')

output = data.frame(year = NA, matedate = NA)
for (y in 2002:2019) {
  
  tmp = getEbirdBarchartData(countyCode = 'CA-ON-NP', species = 'reevir1', year = y)
  
  output = rbind(output, data.frame(year = y, matedate = matedateCalc2(tmp)))
}