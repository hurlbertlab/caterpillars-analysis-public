source("C:/git/caterpillars-analysis-public/code/summaryStats.r")

par(mfrow = c(1, 2), mar = c(6, 6, 1, 6), mgp = c(3, 1, 0), cex.lab = 1.5, cex.axis = 1, las = 1, tck = -0.05)

projectTrends(plot = T, plotVar = 'cumNusers', type = 'l', ylab = "Number of users", lwd = 3, col = 'skyblue4')
par(new=T)
projectTrends(plot = T, plotVar = 'cumNsites', type = 'l', ylab = "", lwd = 3, col = 'goldenrod1', yaxt = "n")
axis(4)
mtext("Number of sites", 4, line = 3, cex = 1.5, las = 0)

projectTrends(plot = T, plotVar = 'cumNsurvs', type = 'l', ylab = "Thousands of surveys", lwd = 3, col = 'limegreen', scalar = .001)
par(new=T)
projectTrends(plot = T, plotVar = 'cumNcats', type = 'l', ylab = "", lwd = 3, col = 'salmon', yaxt = "n", scalar = .001)
axis(4)
mtext("Thousands of caterpillars", 4, line = 3, cex = 1.5, las = 0)



par(mfrow =c(1,1), tck = -0.03)
projectTrends(plot = T, plotVar = 'cumNsites', type = 'l', ylab = "Cumulative number of sites", lwd = 3, col = 'goldenrod1', ylim = c(0, 200), col.lab = 'goldenrod1')
par(new = T)
projectTrends(plot = T, plotVar = 'nSites', type = 'l', ylab = "", yaxt = "n", lwd = 3, col = 'dodgerblue', scalar = 1)
axis(4)
mtext("Number of sites", 4, line = 2.5, cex = 1.5, las = 0, col = "dodgerblue")
legend("topleft", legend = c("cumulative", "by year"), lwd = 3, col = c("goldenrod1", "dodgerblue"), bty = "n")
