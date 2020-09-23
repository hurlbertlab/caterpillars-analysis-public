### Hierarchical clustering of iNat user taxonomy

library(tidyverse)
library(factoextra)
library(cluster)

# Append correct BioArk path

info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\BioArk")

# Read in data

inat_user_orders <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_insecta_orders_2019.csv"), stringsAsFactors = F)

inat_user_classes <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_classes_2019.csv"), stringsAsFactors = F)

# Only users with at least 20 obs

user_orders <- inat_user_orders %>%
  group_by(user.login) %>%
  mutate(obs = sum(total_obs)) %>%
  filter(obs > 20) %>%
  ungroup() %>%
  mutate(prop_obs = total_obs/obs) %>%
  dplyr::select(user.login, order, prop_obs)

user_classes <- inat_user_classes %>%
  group_by(user.login) %>%
  mutate(obs = sum(total_obs)) %>%
  filter(obs > 20) %>%
  ungroup() %>%
  mutate(prop_obs = total_obs/obs) %>%
  dplyr::select(user.login, class, prop_obs)

# Reshape data to wide format

inat_orders_wide <- pivot_wider(user_orders, values_from = prop_obs, names_from = order)
write.csv(inat_orders_wide, "data/inat_user_insecta_orders_wide.csv", row.names = F)

inat_classes_wide <- pivot_wider(user_classes, values_from = prop_obs, names_from = class)
write.csv(inat_classes_wide, "data/inat_user_classes_wide.csv", row.names = F)

inat_orders_wide <- read.csv("data/inat_user_insecta_orders_wide.csv", stringsAsFactors = F)

inat_classes_wide <- read.csv("data/inat_user_classes_wide.csv", stringsAsFactors = F) %>%
  dplyr::select(-NA.)

# Distance matrix

orders_dist <- dist(inat_orders_wide)

classes_dist <- dist(inat_classes_wide)

# Agglomerative cluserting
clust <- hclust(orders_dist)

fviz_nbclust(user_orders, FUN = hcut, method = "silhouette")

plot(clust)
rect.hclust(clust, k = 4, border = 2:5)

sub_grp <- cutree(clust, k = 4)

orders_df <- inat_orders_wide %>%
  mutate(cluster = sub_grp)

clust_ag <- agnes(orders_dist, method = "complete")

# Divisive clustering

clust_di <- diana(orders_dist)



