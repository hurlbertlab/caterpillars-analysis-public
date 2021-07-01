### iNaturalist user behavior MS
## Figures and analysis

## Earlier iterations of plot code in inat_user_behavior.R
## Clean data for plots from iNat databases in inat_ms_data_cleaning.R
## Save figures to caterpillars-analysis-public/figs/inaturalist

#### Libraries ####

library(tidyverse)
library(cowplot)
library(sf)
library(tmap)
library(maptools)
library(ggExtra)
library(factoextra)
library(cluster)

## ggplot theme
theme_set(theme_classic(base_size = 18))

# Append correct BioArk path

info <- sessionInfo()
bioark <- ifelse(grepl("apple", info$platform), "/Volumes", "\\\\ad.unc.edu/bio")

#### Figure 3: Number of species vs number of observations, observations by user acct ####

user_even_order <- read.csv("iNatUserBehavior/data/inat_user_evenness_orders.csv", stringsAsFactors = F)
user_even_class <- read.csv("iNatUserBehavior/data/inat_user_evenness_class.csv", stringsAsFactors = F)

inat_user_spp <- read.csv("iNatUserBehavior/data/inat_thru2019_user_spp.csv", stringsAsFactors = F)
inat_user_obs <- read.csv("iNatUserBehavior/data/inat_thru2019_user_obs.csv", stringsAsFactors = F)

user_specializ <- inat_user_obs %>%
  left_join(inat_user_spp) %>%
  left_join(user_even_class) %>%
  left_join(user_even_order, by = c("user.login"), suffix = c("_class", "_order")) %>%
  mutate_at(c("shannonE_order", "shannonE_class"), ~ifelse(. == 0, NA, .)) %>%
  mutate(spp_per_obs = n_spp/total_obs,
         spp_per_obs_insect = n_spp_insect/total_obs_insect) %>%
  filter(spp_per_obs <= 1, spp_per_obs_insect <= 1) 

spp_obs_plot <- ggplot(user_specializ, aes(x = total_obs, y = n_spp)) + geom_point(alpha = 0.1) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) +
  scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  geom_smooth(se = F, col = "lightblue") +
  labs(x = "Observations per user", y = "Species per user")

spp_hist <- ggplot(user_specializ, aes(x = n_spp)) + geom_histogram(bins = 20) +
  geom_vline(xintercept = median(user_specializ$n_spp, na.rm = T), col = "red") +
  scale_x_log10() + theme_void() + coord_flip()

obs_hist <- ggplot(user_specializ, aes(x = total_obs)) + geom_histogram(bins = 20) +
  geom_vline(xintercept = median(user_specializ$total_obs, na.rm = T), col = "red") +
  scale_x_log10() + theme_void()

align_x_hist <- align_plots(obs_hist, spp_obs_plot, align = "v")[[1]]
align_y_hist <- align_plots(spp_hist, spp_obs_plot, align = "h")[[1]]

panel1 <- plot_grid(align_x_hist, NULL, spp_obs_plot, align_y_hist,
          ncol = 2, nrow = 2, rel_heights = c(0.2, 1), rel_widths = c(1, 0.2))

user_prop_obs <- inat_user_obs %>%
  mutate(all_obs = sum(total_obs),
         prop_obs = total_obs/all_obs) %>%
  arrange(total_obs) %>%
  mutate(cum_obs = cumsum(prop_obs))

# 90, 99th percentiles

quantile(user_prop_obs$total_obs, c(0.9, 0.99))
# 29, 455

sum(user_prop_obs$prop_obs[user_prop_obs$total_obs >= 29])
sum(user_prop_obs$prop_obs[user_prop_obs$total_obs >= 455])

panel2 <- ggplot(user_prop_obs, aes(x = total_obs, y = cum_obs)) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000)) + geom_line() +
  geom_ribbon(data = filter(user_prop_obs, total_obs >= 29), aes(x = total_obs, ymin = 0, ymax = cum_obs), alpha = 0.3, fill = "skyblue") +
  geom_ribbon(data = filter(user_prop_obs, total_obs >= 455), aes(x = total_obs, ymin = 0, ymax = cum_obs), alpha = 0.5, fill = "skyblue") +
  annotate(geom = "text", x = 650, y = 0.1, label = "Top 10% of users provide\n87% of observations", size = 4) +
  geom_segment(aes(x=29,xend=max(user_prop_obs$total_obs),y=0.02,yend=0.02), cex = 0.5, lineend = "butt") +
  geom_segment(aes(x=455,xend=max(user_prop_obs$total_obs),y=0.22,yend=0.22), col = "navy", lineend = "butt") +
  annotate(geom = "text", x = 8200, y = 0.29, label = "Top 1% of users provide\n62% of observations", size = 4, col = "navy") +
  labs(x = "Observations per user", y = "Proportion of all observations")

plot_grid(panel1, panel2, nrow = 1, labels = c("A", "B"))

ggsave("iNatUserBehavior/figs/fig3_nSpp_vs_nObs.pdf", units = "in", width = 10, height = 5)

##### Figure 4 & 5: All classes taxonomic clustering; Insecta orders taxonomic clustering ######

# Read in data

inat_user_orders <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_insecta_orders_thru2019.csv"), stringsAsFactors = F)

inat_user_classes <- read.csv(paste0(bioark, "/hurlbertlab/DiCecco/data/inat_user_behavior/inat_user_obs_classes_thru2019.csv"), stringsAsFactors = F)

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
# write.csv(inat_orders_wide, "iNatUserBehavior/data/inat_user_insecta_orders_wide.csv", row.names = F)
# 
# inat_classes_wide <- pivot_wider(user_classes, values_from = prop_obs, names_from = class)
# write.csv(inat_classes_wide, "iNatUserBehavior/data/inat_user_classes_wide.csv", row.names = F)

inat_orders_wide <- read.csv("iNatUserBehavior/data/inat_user_insecta_orders_wide.csv", stringsAsFactors = F)

inat_classes_wide <- read.csv("iNatUserBehavior/data/inat_user_classes_wide.csv", stringsAsFactors = F)

# Shannon evenness

evenness_null <- read.csv("iNatUserBehavior/data/inat_evenness_null_mod.csv", stringsAsFactors = F)

# Distance matrix

orders_dist <- dist(inat_orders_wide)

classes_dist <- dist(inat_classes_wide)

# Agglomerative clustering Insects

clust <- hclust(orders_dist)

# Clustering for classes (obs > 50)

clust_classes <- hclust(classes_dist)

# Proportion of observations by class

user_obs <- inat_user_classes %>%
  filter(class %in% classes$class) %>%
  group_by(user.login) %>%
  summarize(obs = sum(total_obs))

class_total_obs <- sum(user_obs$obs)

## Figure 4: Classes

g <- 10

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

paired_cols <- RColorBrewer::brewer.pal(10, "Paired")

class_cols <- data.frame(class = c("Liliopsida", "Magnoliopsida", 
                                   "Agaricomycetes", "Arachnida",
                                   "Insecta","Actinopterygii",
                                   "Amphibia", "Reptilia", "Aves","Mammalia"),
                         col = c("#B2DF8A", "#33A02C", "#FB9A99",
                                 "#E31A1C", "#FDBF6F", "#FF7F00",
                                 "#CAB2D6", "#6A3D9A", "#A6CEE3","#1F78B4"), stringsAsFactors = F)

cols <- class_cols$col
names(cols) <- class_cols$class

class_grp_plot <- taxon_grps %>%
  left_join(top_12_class) %>%
  group_by(class_plot, cluster) %>%
  summarize(mean = sum(mean_prop_scaled)) %>%
  left_join(pct_users) %>%
  mutate(group_label = factor(rev_rank, levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"))) 



cluster_plot <- ggplot(class_grp_plot, aes(x = group_label, y = mean, 
                                           fill = fct_relevel(class_plot, class_cols$class))) + 
  geom_col(position = "stack") + scale_fill_manual(values = cols) + 
  labs(x = "Group", y = "Mean proportion of observations", fill = "Class") + coord_flip() +
  theme(legend.position = "left")

group_evenness <- groups_classes %>%
  left_join(evenness_null) %>%
  left_join(pct_users)

dens_plot <- ggplot(group_evenness, aes(x = shannonE_class_z_obs,)) +
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
  geom_text(aes(x = -25, y = 0.1, 
                label = ifelse(round(pct_user, 1) > 50, paste0(round(pct_user, 1), "% of users"),
                               ifelse(round(pct_user, 1) < 1, paste0("<1%"), paste0(round(pct_user, 1), "%"))), 
                size = 6)) +
  geom_text(aes(x = 5, y = 0.1, 
                label = ifelse(n_user > 20000, paste0("n = ", n_user), paste0(n_user)), 
                size = 6)) +
  theme(plot.margin = unit(c(0.5, 0, 0.25, 0), "in"))

plot_grid(cluster_plot, dens_plot, nrow = 1, rel_widths = c(0.55, 0.45), labels = c("A", "B"), label_size = 16)
ggsave(paste0("iNatUserBehavior/figs/fig4_class_group_multipanel_", g, ".pdf"), units = "in", height = 6, width = 14)

## Figure 5: Insecta

g <- 10

sub_grp <- cutree(clust, k = g)

orders_df <- inat_orders_wide %>%
  mutate(cluster = sub_grp)

groups_orders <- orders_df %>%
  dplyr::select(user.login, cluster) 

pct_users <- groups_orders %>%
  group_by(cluster) %>%
  summarize(n_user = n_distinct(user.login)) %>%
  filter(n_user > 10) %>%
  mutate(total_user = sum(n_user),
          prop_user = n_user/total_user,
          pct_user = prop_user*100,
          rank = row_number(pct_user),
          rev_rank = row_number(desc(pct_user))) 

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
  left_join(pct_users) %>% 
  filter(!is.na(rank)) %>%
  mutate(group_label = factor(rev_rank, levels = c("8", "7", "6", "5", "4", "3", "2", "1")))

cluster_plot <- ggplot(order_grp_plot, aes(x = group_label, y = mean_prop, fill = order_plot)) + 
  geom_col(position = "stack") + scale_fill_brewer(palette = "Paired") + 
  labs(x = "Group", y = "Mean proportion of observations", fill = "Order") + coord_flip() +
  theme(legend.position = "left")

group_evenness <- groups_orders %>%
  left_join(evenness_null) %>%
  left_join(pct_users) %>%
  filter(!is.na(rank))

dens_plot <- ggplot(group_evenness, aes(x = shannonE_order_z_obs)) + 
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
                label = ifelse(round(pct_user, 1) > 50, paste0(round(pct_user, 1), "% of users"),
                               ifelse(round(pct_user, 1) < 1, paste0("<1%"), paste0(round(pct_user, 1), "%"))), 
                size = 6)) +
  geom_text(aes(x = 6, y = 0.1, 
                label = ifelse(n_user < 20, paste0("n = ", n_user), paste0(n_user)), 
                size = 6)) +
  theme(plot.margin = unit(c(0.5, 0, 0.25, 0), "in"))

plot_grid(cluster_plot, dens_plot, nrow = 1, rel_widths = c(0.55, 0.45), labels = c("A", "B"), label_size = 16)
ggsave(paste0("iNatUserBehavior/figs/fig5_insect_order_group_multipanel_", g, ".pdf"), units = "in", height = 6, width = 14)
