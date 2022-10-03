# Script for displaying caterpillar phenology across sites

library(dplyr)
library(lubridate)

source('code/analysis_functions.r')
source('code/reading_datafiles_without_users.r')



###################################################
# NC Botanical garden thru time
pr = fullDataset %>%
  filter(Name == "Prairie Ridge Ecostation") 

pdf('figs/pr_caterpillar_biomass_pheno_2015.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015, lwd = 5, col = c('purple2'), bty = 'n')
dev.off()


pdf('figs/pr_caterpillar_biomass_pheno_2015-2016.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2016, lwd = 5, col = c('purple2', 'blue'), bty = 'n')
dev.off()


pdf('figs/pr_caterpillar_biomass_pheno_2015-2017.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_17_biomass = pr %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2017, lwd = 5, col = c('purple2', 'blue', 'skyblue'), bty = 'n')
dev.off()


pdf('figs/pr_caterpillar_biomass_pheno_2015-2018.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_17_biomass = pr %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_18_biomass = pr %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2018, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink'), bty = 'n')
dev.off()



pdf('figs/pr_caterpillar_biomass_pheno_2015-2019.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_17_biomass = pr %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_18_biomass = pr %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_19_biomass = pr %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = 2015:2019, lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon'), bty = 'n')
dev.off()







pdf('figs/pr_caterpillar_biomass_pheno_2015-2021.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_17_biomass = pr %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_18_biomass = pr %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_19_biomass = pr %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_21_biomass = pr %>%
  filter(Year == 2021) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'red', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))
jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = c(2015:2019, 2021), lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon', 'red'))
dev.off()


pdf('figs/pr_caterpillar_biomass_pheno_2015-2022.pdf', height = 5, width = 7)
par(mar = c(4, 6, 1, 2))
pr_15_biomass = pr %>%
  filter(Year == 2015) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'purple2', 
                    plot = TRUE, allDates = TRUE, lwd = 5, xlab = "", ylab = "", xaxt = "n", 
                    xlim = c(135, 210), ylim = c(0, 23), jdRange = c(135, 210))

pr_16_biomass = pr %>%
  filter(Year == 2016) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'blue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_17_biomass = pr %>%
  filter(Year == 2017) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'skyblue', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_18_biomass = pr %>%
  filter(Year == 2018) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'lightpink', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_19_biomass = pr %>%
  filter(Year == 2019) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'salmon', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_21_biomass = pr %>%
  filter(Year == 2021) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'red', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

pr_22_biomass = pr %>%
  filter(Year == 2022) %>%
  meanDensityByWeek(ordersToInclude = 'caterpillar', plotVar = 'fracSurveys', col = 'darkred', new = FALSE,
                    plot = TRUE, allDates = TRUE, lwd = 5, jdRange = c(135, 210))

jdAxis(c(135, 210), biweekly = TRUE, cex.axis = 1.3)
mtext("Caterpillar biomass (mg / survey)", 2, line = 3, cex = 1.5)

legend("topleft", legend = c(2015:2019, 2021, 2022), lwd = 5, col = c('purple2', 'blue', 'skyblue', 'lightpink', 'salmon', 'red', 'darkred'))
dev.off()




