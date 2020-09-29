### Hierarchical clustering of iNat user taxonomy

library(tidyverse)
library(factoextra)
library(cluster)
library(cowplot)

# ggplot theme

theme_set(theme_classic(base_size = 18))

# Append correct BioArk path

info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\BioArk")

# Read in data

inat_user_orders <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_insecta_orders_2019.csv"), stringsAsFactors = F)

inat_user_classes <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_classes_2019.csv"), stringsAsFactors = F)

# Observations per order and class

orders <- inat_user_orders %>%
  group_by(order) %>%
  summarize(obs = sum(total_obs)) %>%
  filter(obs > 1000)

classes <- inat_user_classes %>%
  group_by(class) %>%
  summarize(obs = sum(total_obs)) %>%
  filter(obs > 5000)

# Only users with at least 20 obs, orders with at least 1000 observations, classes with at least 5000 observations

user_orders <- inat_user_orders %>%
  group_by(user.login) %>%
  mutate(obs = sum(total_obs)) %>%
  filter(obs > 20, order %in% orders$order) %>%
  ungroup() %>%
  mutate(prop_obs = total_obs/obs) %>%
  dplyr::select(user.login, order, prop_obs)

user_classes <- inat_user_classes %>%
  group_by(user.login) %>%
  mutate(obs = sum(total_obs)) %>%
  filter(obs > 20, class %in% classes$class) %>%
  ungroup() %>%
  mutate(prop_obs = total_obs/obs) %>%
  dplyr::select(user.login, class, prop_obs)

# Reshape data to wide format

inat_orders_wide <- pivot_wider(user_orders, values_from = prop_obs, names_from = order)
write.csv(inat_orders_wide, "data/inat_user_insecta_orders_wide.csv", row.names = F)

inat_classes_wide <- pivot_wider(user_classes, values_from = prop_obs, names_from = class)
write.csv(inat_classes_wide, "data/inat_user_classes_wide.csv", row.names = F)

inat_orders_wide <- read.csv("data/inat_user_insecta_orders_wide.csv", stringsAsFactors = F)

inat_classes_wide <- read.csv("data/inat_user_classes_wide.csv", stringsAsFactors = F)

# Distance matrix

orders_dist <- dist(inat_orders_wide)

classes_dist <- dist(inat_classes_wide)

# Agglomerative clustering Insects

clust <- hclust(orders_dist)

# Explore 8, 10, 12 groups for Insects

groups <- c(8, 10, 12)

for(g in groups) {
  sub_grp <- cutree(clust, k = g)
  
  orders_df <- inat_orders_wide %>%
    mutate(cluster = sub_grp)
  
  groups_orders <- orders_df %>%
    dplyr::select(user.login, cluster)
  
  n_users_plot <- ggplot(groups_orders, aes(x = cluster)) + geom_histogram() + labs(x = "Group", y = "Number of users") +
    scale_y_log10() + scale_x_continuous(breaks = c(1:g))
  
  taxon_grps <- user_orders %>%
    left_join(groups_orders) %>%
    group_by(cluster, order) %>%
    summarize(mean_prop = mean(prop_obs))
  
  top_12_order <- taxon_grps %>%
    group_by(order) %>%
    summarize(total_mean = mean(mean_prop)) %>%
    arrange(desc(total_mean)) %>%
    mutate(order_plot = ifelse(row_number() > 11, "Other", order))
  
  order_grp_plot <- taxon_grps %>%
    left_join(top_12_order)
  
  taxon_grp_plot <- ggplot(order_grp_plot, aes(x = cluster, y = mean_prop, fill = order_plot)) + 
    geom_col(position = "stack") + scale_fill_brewer(palette = "Paired") + scale_x_continuous(breaks = c(1:g)) +
    labs(x = "Group", y = "Mean proportion of observations", fill = "Order")
  
  plot_grid(n_users_plot, taxon_grp_plot, nrow = 1, rel_widths = c(0.4, 0.6))
  ggsave(paste0("figs/inaturalist/insect_order_user_groups_", g, ".pdf"), units = "in", height = 5, width = 11)
}

# Clustering for classes
# Try on desktop at school or longleaf

clust_classes <- hclust(classes_dist)
