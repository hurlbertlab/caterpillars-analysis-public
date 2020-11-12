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
# More stringent class filter

orders <- inat_user_orders %>%
  group_by(order) %>%
  summarize(obs = sum(total_obs)) %>%
  filter(obs > 1000)

classes <- inat_user_classes %>%
  group_by(class) %>%
  summarize(obs = sum(total_obs)) %>%
  arrange(desc(obs)) %>%
  slice(1:10)

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
  filter(obs > 50, class %in% classes$class) %>%
  ungroup() %>%
  mutate(prop_obs = total_obs/obs) %>%
  dplyr::select(user.login, class, prop_obs)

# Reshape data to wide format

# inat_orders_wide <- pivot_wider(user_orders, values_from = prop_obs, names_from = order)
# write.csv(inat_orders_wide, "data/inat_user_insecta_orders_wide.csv", row.names = F)
# 
# inat_classes_wide <- pivot_wider(user_classes, values_from = prop_obs, names_from = class)
# write.csv(inat_classes_wide, "data/inat_user_classes_wide.csv", row.names = F)

inat_orders_wide <- read.csv("data/inat_user_insecta_orders_wide.csv", stringsAsFactors = F)

inat_classes_wide <- read.csv("data/inat_user_classes_wide.csv", stringsAsFactors = F)

# Shannon evenness

evenness_null <- read.csv("data/inat_evenness_null_mod.csv", stringsAsFactors = F)

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
  
  pct_users <- groups_orders %>%
    group_by(cluster) %>%
    summarize(n_user = n_distinct(user.login)) %>%
    mutate(total_user = sum(n_user),
           prop_user = n_user/total_user,
           pct_user = prop_user*100,
           rank = dense_rank(pct_user),
           rev_rank = dense_rank(desc(pct_user)))
  
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
    left_join(top_12_order) %>%
    left_join(pct_users) 
  
  cluster_plot <- ggplot(order_grp_plot, aes(x = rank, y = mean_prop, fill = order_plot)) + 
    geom_col(position = "stack") + scale_fill_brewer(palette = "Paired") + scale_x_continuous(breaks= c(1:g)) +
    labs(x = "Group", y = "Mean proportion of observations", fill = "Order") + coord_flip() +
    theme(legend.position = "left")
  # ggsave(paste0("figs/inaturalist/insect_order_user_groups_", g, ".pdf"), units = "in", height = 5, width = 8)
  
  group_evenness <- groups_orders %>%
    left_join(evenness_null) %>%
    left_join(pct_users)
  
  dens_plot <- ggplot(group_evenness, aes(x = shannonE_order_z_spp)) + 
    geom_density(alpha = 0.5, fill = "gray") +
    labs(y = "", fill = "Group") +
    xlab(expression(""%<-%"More specialist users       More generalist users"%->%"")) +
    xlim(-35, 10) +
    geom_vline(xintercept = 0, lty = 2) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~rev_rank, ncol = 1) + theme(strip.background = element_blank(),
                                           strip.text.x = element_blank(), legend.position = "none",
                                           axis.line.y = element_blank(), axis.text.y = element_blank(),
                                           axis.ticks.y = element_blank()) +
    geom_text(aes(x = -20, y = 0.15, 
                  label = ifelse(round(pct_user, 1) < 1, paste0("<1%"), paste0(round(pct_user, 1), "%")), size = 5))
  # ggsave(paste0("figs/inaturalist/insect_order_groups_shannonE_", g, ".pdf"), units = "in", height = 10, width = 6)

  
  plot_grid(cluster_plot, dens_plot, nrow = 1, rel_widths = c(0.55, 0.45), labels = c("A", "B"), label_size = 16)
  ggsave(paste0("figs/inaturalist/insect_order_group_multipanel_", g, ".pdf"), units = "in", height = 6, width = 14)
}

# Clustering for classes (works with obs > 50)

clust_classes <- hclust(classes_dist)

# Proportion of observations by class

user_obs <- inat_user_classes %>%
  filter(class %in% classes$class) %>%
  group_by(user.login) %>%
  summarize(obs = sum(total_obs))

class_total_obs <- sum(user_obs$obs)

# Explore 8, 10, 12 groups for Classes

groups <- c(8, 10, 12)

for(g in groups) {
  sub_grp <- cutree(clust_classes, k = g)
  
  classes_df <- inat_classes_wide %>%
    mutate(cluster = sub_grp)
  
  groups_classes <- classes_df %>%
    dplyr::select(user.login, cluster) 
  
  pct_users <- groups_classes %>%
    left_join(user_obs) %>%
    group_by(cluster) %>%
    summarize(n_user = n_distinct(user.login),
              prop_all_obs = 100*sum(obs)/class_total_obs) %>%
    mutate(total_user = sum(n_user),
           prop_user = n_user/total_user,
           pct_user = prop_user*100,
           rank = dense_rank(pct_user),
           rev_rank = dense_rank(desc(pct_user)))
  
  taxon_grps <- user_classes %>%
    left_join(groups_classes) %>%
    group_by(cluster, class) %>%
    summarize(mean_prop = mean(prop_obs)) %>%
    group_by(cluster) %>%
    mutate(total = sum(mean_prop)) %>%
    mutate(mean_prop_scaled = mean_prop/total)
  
  top_12_class <- taxon_grps %>%
    group_by(class) %>%
    summarize(total_mean = mean(mean_prop_scaled)) %>%
    arrange(desc(total_mean)) %>%
    mutate(class_plot = ifelse(row_number() > 11, "Other", class))
  
  class_grp_plot <- taxon_grps %>%
    left_join(top_12_class) %>%
    group_by(class_plot, cluster) %>%
    summarize(mean = sum(mean_prop_scaled)) %>%
    left_join(pct_users)
  
  cluster_plot <- ggplot(class_grp_plot, aes(x = rank, y = mean, fill = class_plot)) + 
    geom_col(position = "stack") + scale_fill_brewer(palette = "Paired") + scale_x_continuous(breaks = c(1:g)) +
    labs(x = "Group", y = "Mean proportion of observations", fill = "Class") + coord_flip() +
    theme(legend.position = "left")
  # ggsave(paste0("figs/inaturalist/class_user_groups_", g, ".pdf"), units = "in", height = 5, width = 8.5)
  
  group_evenness <- groups_classes %>%
    left_join(evenness_null) %>%
    left_join(pct_users)
  
  dens_plot <- ggplot(group_evenness, aes(x = shannonE_class_z_spp,)) +
    geom_density(alpha = 0.5, fill = "gray") +
    labs(y = "", fill = "Group") +
    xlab(expression(""%<-%"More specialist users       More generalist users"%->%"")) +
    xlim(-35, 10) +
    geom_vline(xintercept = 0, lty = 2) +
    scale_fill_brewer(palette = "Paired") +
    facet_wrap(~rev_rank, ncol = 1) + theme(strip.background = element_blank(),
                                            strip.text.x = element_blank(), legend.position = "none",
                                            axis.line.y = element_blank(), axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank()) +
    geom_text(aes(x = -20, y = 0.1, 
                  label = ifelse(round(pct_user, 1) < 1, paste0("<1%"), paste0(round(pct_user, 1), "%")), size = 5))
  
  # ggsave(paste0("figs/inaturalist/class_groups_shannonE_", g, ".pdf"), units = "in", height = 5, width = 6)
  
  plot_grid(cluster_plot, dens_plot, nrow = 1, rel_widths = c(0.55, 0.45), labels = c("A", "B"), label_size = 16)
  ggsave(paste0("figs/inaturalist/class_group_multipanel_", g, ".pdf"), units = "in", height = 6, width = 14)
  
  
}
