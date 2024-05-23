# Script for exploring and comparing data submitted by Hurlbert Lab members

# Load libraries
library(dplyr)
library(vioplot)


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
fullDataset = read.csv("data/fullDataset_2024-05-23.csv")

people = data.frame(UserFKOfObserver = c(26, 3661, 4035, 4036, 4062),
                    UserName = c("Allen", "Ivara", "Grace", "Alex", "Bella"))

# Filter the full dataset down to just the surveys we have done
fd = fullDataset %>% 
  filter(UserFKOfObserver %in% people$UserFKOfObserver,
         Year == 2024) %>%
  left_join(people, by = 'UserFKOfObserver')

arthGroups = c("ant", "aphid", "bee", "beetle", "caterpillar", 
               "daddylonglegs", "fly", "grasshopper", "leafhopper",
               "moths", "spider", "truebugs")


## Arthropod size by observer

par(mfrow = c(6, 2), mar = c(3, 3, 2, 1), oma = c(4, 4, 0, 0), 
    mgp = c(2.5, 1, 0), tck = -0.01)
for (a in arthGroups) {
  
  tmp = filter(fd, Group == a)
  vioplot(tmp$Length ~ tmp$UserName, ylab = 'Length', 
          xlab = '', main = a, las = 1)
  
}


