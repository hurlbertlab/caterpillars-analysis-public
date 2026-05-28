# Script for exploring and comparing data submitted by Hurlbert Lab members

# Load libraries
library(dplyr)
library(vioplot)
library(tidyr)

# Update the raw data files by 
#   1) opening the caterpillars-count-data repo,
#   2) running the update_catcount_data.R script (you can simply click the 'Source' button)
#   3) run the function updateCatCountData() in the console,
#   4) in the Git tab, select the checkboxes for all files (you can simply click
#      the first file and then hold down shift while clicking the last one to select all)
#   5) click Commit, and enter a note that you are updating data files,
#   6) click Push, and ok to subsequent messages.

# Now create an updated fullDataset file by running the script called
# 'reading_datafiles_without_users.r'

# Update the date in the filename on the next line to read in this latest version.
fullDataset = read.csv("data/fullDataset_2026-05-28.csv")

people = data.frame(UserFKOfObserver = c(26, 3661, 4369, 5281, 5201),
                    UserName = c("Allen", "Ivara", "Nosa", "Oliver", "Sophia"))

# Filter the full dataset down to just the surveys we have done
fd = fullDataset %>% 
  filter(UserFKOfObserver %in% people$UserFKOfObserver,
         Year == 2026) %>%
  left_join(people, by = 'UserFKOfObserver')

arthGroups = c("ant", "aphid", "bee", "beetle", "caterpillar", 
               "daddylonglegs", "fly", "grasshopper", "leafhopper",
               "moths", "spider", "truebugs")


## Arthropod size by observer

par(mfrow = c(3, 2), mar = c(3, 3, 2, 1), oma = c(4, 4, 0, 0), 
    mgp = c(2.5, 1, 0), tck = -0.01)

for (a in arthGroups) {
  
  tmp = filter(fd, Group == a)
  vioplot(tmp$Length ~ tmp$UserName, ylab = 'Length', 
          xlab = '', main = a, las = 1, cex.axis = 1.1)
  
}


# Herbivory scores by observer

herb = fd %>%
  group_by(UserName) %>%
  summarize(nSurvs = n_distinct(ID),
            n0 = n_distinct(ID[HerbivoryScore == 0]),
            n1 = n_distinct(ID[HerbivoryScore == 1]),
            n2 = n_distinct(ID[HerbivoryScore == 2]),
            n3 = n_distinct(ID[HerbivoryScore == 3]),
            n4 = n_distinct(ID[HerbivoryScore == 4]),
            none = n0/nSurvs,
            trace = n1/nSurvs,
            light = n2/nSurvs,
            moderate = n3/nSurvs,
            heavy = n4/nSurvs)
  

herb.mat <- as.matrix(herb[, c("none", "trace", "light", "moderate", "heavy")])
  
# Optional colors
cols <- c("gray70", "skyblue", "orange", "red", "darkred")
  
par(mfrow = c(1,1))
  # Stacked barplot
barplot(
  t(herb.mat),                         # transpose so categories stack within users
  names.arg = herb$UserName,
  col = cols,
  xlab = "User",
  ylab = "Percent Herbivory",
  legend.text = colnames(herb.mat),
  args.legend = list(x = "right"),
  border = "white"
)  
  
  
# Percent of surveys with each type of arthropod by user
foo = fd %>% 
  group_by(UserName) %>%
  summarize(nSurvs = n_distinct(ID),
            nCats = n_distinct(ID[Group == 'caterpillar'], na.rm = T),
            nSpiders = n_distinct(ID[Group == 'spider'], na.rm = T),
            nBeets = n_distinct(ID[Group == 'beetle'], na.rm = T),
            nBugs = n_distinct(ID[Group == 'truebugs'], na.rm = T),
            nHops = n_distinct(ID[Group == 'leafhopper'], na.rm = T),
            nAphids = n_distinct(ID[Group == 'aphid'], na.rm = T),
            pctCat = 100*nCats/nSurvs,
            pctSpi = 100*nSpiders/nSurvs,
            pctBeet = 100*nBeets/nSurvs,
            pctBug = 100*nBugs/nSurvs,
            pctHop = 100*nHops/nSurvs,
            pctAph = 100*nAphids/nSurvs)

pct.mat <- as.matrix(foo[, c("pctCat", "pctSpi", "pctBeet",
                             "pctBug", "pctHop", "pctAph")])

# Transpose so each arthropod group becomes a separate bar
# within each UserName group
pct.mat <- t(pct.mat)

# Colors for the 6 arthropod groups
cols <- c("forestgreen", "orange", "gold",
          "skyblue", "purple", "red")


par(mar = c(5, 4, 1, 1), mgp = c(3, 1, 0))
# Grouped barplot
barplot(
  pct.mat,
  beside = TRUE,
  names.arg = foo$UserName,
  col = cols,
  ylim = c(0, 35),
  ylab = "Percent of surveys",
  xlab = "User",
  legend.text = c('caterpillar', 'spider', 'beetle', 'true bug', 'leafhopper', 'aphid'),
  args.legend = list(x = "topright", bty = "n"),
  las = 2   # rotate usernames if needed
)


