# inaturalist descriptive MS
# Null expectations of Shannon's Evenness for users with >20 observations

library(dplyr)
library(purrr)

setwd("/proj/hurlbertlab/gdicecco/caterpillars-analysis/")

#### Calculate null expected Shannon evenness -- longleaf ####

# For range of n_obs observed by users, calculate 999 shannon Evenness, z score for actual
# Weighted by # observations, weighted by # species
# 218 classes, 27 Insect orders in data

user_even_order <- read.csv("inat_user_evenness_orders.csv", stringsAsFactors = F)
user_even_class <- read.csv("inat_user_evenness_class.csv", stringsAsFactors = F)

inat_user_spp <- read.csv("inat_thru2019_user_spp.csv", stringsAsFactors = F)
inat_user_obs <- read.csv("inat_thru2019_user_obs.csv", stringsAsFactors = F)

user_specializ <- inat_user_obs %>%
  left_join(inat_user_spp) %>%
  left_join(user_even_class) %>%
  left_join(user_even_order, by = c("user.login"), suffix = c("_class", "_order")) %>%
  filter(shannonE_order != 0, shannonE_class != 0) %>%
  # mutate_at(c("shannonE_order", "shannonE_class"), ~ifelse(. == 0, NA, .)) %>%
  mutate(spp_per_obs = n_spp/total_obs,
         spp_per_obs_insect = n_spp_insect/total_obs_insect) %>%
  filter(spp_per_obs <= 1, spp_per_obs_insect <= 1)
  # mutate_at(c("spp_per_obs", "spp_per_obs_insect"), ~ifelse(. == 1, . - 0.00001, .))

inat_species <- read.csv("inat_taxon_info_thru2019.csv", stringsAsFactors = F)

inat_class_obs <- inat_species %>%
  group_by(class) %>%
  summarize(obs = sum(total_obs),
            spp = n_distinct(taxon_name))

inat_order_obs <- inat_species %>%
  filter(class == "Insecta") %>%
  group_by(order) %>%
  summarize(obs = sum(total_obs),
            spp = n_distinct(taxon_name))

## This takes ~ 5 days to run

shannon_e_expected <- user_specializ %>%
  filter(total_obs > 20) %>%
  mutate(shannonE_class_z_obs = map2_dbl(total_obs, shannonE_class, ~{
    n <- .x
    shannonE <- .y
    
    shannonExpected <- c()
    
    for(i in 1:999) {
      sample <- sample_n(inat_class_obs, n, replace = T, weight = obs) %>%
        group_by(class) %>%
        count()
      
      sum_obs <- sum(sample$n)
      
      shannonH <- -sum((sample$n/sum_obs)*log(sample$n/sum_obs))
      
      shannonE_null <- shannonH/log(218)
      
      shannonExpected <- c(shannonExpected, shannonE_null)
    }
    
    sE_mean <- mean(shannonExpected)
    sE_sd <- sd(shannonExpected)
    
    (shannonE - sE_mean)/sE_sd
    
  }), shannonE_class_z_spp = map2_dbl(total_obs, shannonE_class, ~{
    n <- .x
    shannonE <- .y
    
    shannonExpected <- c()
    
    for(i in 1:999) {
      sample <- sample_n(inat_class_obs, n, replace = T, weight = spp) %>%
        group_by(class) %>%
        count()
      
      sum_obs <- sum(sample$n)
      
      shannonH <- -sum((sample$n/sum_obs)*log(sample$n/sum_obs))
      
      shannonE_null <- shannonH/log(218)
      
      shannonExpected <- c(shannonExpected, shannonE_null)
    }
    
    sE_mean <- mean(shannonExpected)
    sE_sd <- sd(shannonExpected)
    
    (shannonE - sE_mean)/sE_sd
    
  }), shannonE_order_z_obs = map2_dbl(total_obs_insect, shannonE_order, ~{
    n <- .x
    shannonE <- .y
    
    shannonExpected <- c()
    
    for(i in 1:999) {
      sample <- sample_n(inat_order_obs, n, replace = T, weight = obs) %>%
        group_by(order) %>%
        count()
      
      sum_obs <- sum(sample$n)
      
      shannonH <- -sum((sample$n/sum_obs)*log(sample$n/sum_obs))
      
      shannonE_null <- shannonH/log(27)
      
      shannonExpected <- c(shannonExpected, shannonE_null)
    }
    
    sE_mean <- mean(shannonExpected)
    sE_sd <- sd(shannonExpected)
    
    (shannonE - sE_mean)/sE_sd
    
  }), shannonE_order_z_spp = map2_dbl(total_obs_insect, shannonE_order, ~{
    n <- .x
    shannonE <- .y
    
    shannonExpected <- c()
    
    for(i in 1:999) {
      sample <- sample_n(inat_order_obs, n, replace = T, weight = spp) %>%
        group_by(order) %>%
        count()
      
      sum_obs <- sum(sample$n)
      
      shannonH <- -sum((sample$n/sum_obs)*log(sample$n/sum_obs))
      
      shannonE_null <- shannonH/log(27)
      
      shannonExpected <- c(shannonExpected, shannonE_null)
    }
    
    sE_mean <- mean(shannonExpected)
    sE_sd <- sd(shannonExpected)
    
    (shannonE - sE_mean)/sE_sd
    
  }))
Sys.time()

write.csv(shannon_e_expected, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_evenness_null_mod.csv", row.names = F)

# #### Plots ####

# library(tidyverse)
# library(cowplot)
# theme_set(theme_classic(base_size = 18))
# 
# 
# shannon_e_expected <- read.csv("data/inat_evenness_null_mod.csv", stringsAsFactors = F)
# 
# shannon_e_expected_ins <- shannon_e_expected %>%
#   filter(total_obs_insect > 20)
# 
# # Evenness of observations by order/class, z score relative to null expectation
# # Null expectation of Shannon E samples from orders/classes weighted by either observations per order/class or spp per order/class
# 
# ## Distributions: shannonE_z weighted by obs
# 
# obs_dist <- ggplot(shannon_e_expected, aes(x = shannonE_class_z_obs)) +
#   geom_histogram(aes(fill = "All Classes"), alpha = 0.5) +
#   geom_histogram(data = shannon_e_expected_ins, aes(x = shannonE_order_z_obs, fill = "Insect Orders"), alpha = 0.5) +
#   scale_fill_manual(values = c("All Classes" = "skyblue3", "Insect Orders" = "springgreen3")) +
#   labs(x = "z-Shannon Evenness, weighted by observations", y = "Count", fill = "") +
#   theme(legend.position = c(0.8, 0.9))
# 
# ## Distributions: shannonE_z weighted by spp
# 
# spp_dist <- ggplot(shannon_e_expected, aes(x = shannonE_class_z_spp)) +
#   geom_histogram(aes(fill = "All Classes"), alpha = 0.5) +
#   geom_histogram(data = shannon_e_expected_ins, aes(x = shannonE_order_z_spp, fill = "Insect Orders"), alpha = 0.5) +
#   scale_fill_manual(values = c("All Classes" = "skyblue3", "Insect Orders" = "springgreen3")) +
#   labs(x = "z-Shannon Evenness, weighted by species", y = "Count", fill = "") +
#   theme(legend.position = c(0.8, 0.9))
# 
# plot_grid(obs_dist, spp_dist, nrow = 1)
# ggsave("figs/inaturalist/shannon_z_dist.pdf", units = "in", height = 5, width = 12)
# 
# ## For a user: insect vs class weighted by obs
# 
# user_obs <- ggplot(filter(shannon_e_expected, total_obs > 20, total_obs_insect > 20), aes(x = shannonE_class_z_obs, y = shannonE_order_z_obs)) +
#   geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
#   labs(x = "z-Shannon Evenness - All Classes", y = "z-Shannon Evenness - Insect Orders", title = "Weighted by observations")
# 
# user_spp <- ggplot(filter(shannon_e_expected, total_obs > 20, total_obs_insect > 20), aes(x = shannonE_class_z_spp, y = shannonE_order_z_spp)) +
#   geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
#   labs(x = "z-Shannon Evenness - All Classes", y = "z-Shannon Evenness - Insect Orders", title = "Weighted by species")
# 
# plot_grid(user_obs, user_spp, nrow = 1)
# ggsave("figs/inaturalist/shannon_z_order_vs_class.pdf", units = "in", height = 5, width = 12)
# 
# ## Weights
# 
# class_plot <- ggplot(filter(shannon_e_expected, total_obs > 20), aes(x = shannonE_class_z_obs, y = shannonE_class_z_spp)) +
#   geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
#   labs(x = "z-Shannon Evenness - wtd by obs", y = "z-Shannon Evenness - wtd by spp", title = "All classes")
# 
# order_plot <- ggplot(filter(shannon_e_expected, total_obs_insect > 20), aes(x = shannonE_order_z_obs, y = shannonE_order_z_spp)) +
#   geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
#   labs(x = "z-Shannon Evenness - wtd by obs", y = "z-Shannon Evenness - wtd by spp", title = "Insect Orders")
# 
# plot_grid(class_plot, order_plot, nrow = 1)
# ggsave("figs/inaturalist/shannon_z_obs_vs_spp.pdf", units = "in", height = 5, width = 12)
# 
# ## Insect: shannonE_z weighted by obs vs species per obs
# 
# insect_obs <- ggplot(shannon_e_expected_ins, aes(x = shannonE_order_z_obs, y = spp_per_obs_insect)) +
#   geom_point(alpha = 0.1) + geom_smooth(method = "lm", se = F) + ylim(0,1) +
#   labs(x = "z-Shannon Evenness weighted by obs", y = "Species per observation", title = "Insect Orders")
# 
# ## Insect: shannonE_z weighted by spp vs species per obs
# 
# insect_spp <- ggplot(shannon_e_expected_ins, aes(x = shannonE_order_z_spp, y = spp_per_obs_insect)) +
#   geom_point(alpha = 0.1) + geom_smooth(method = "lm", se = F) +  ylim(0,1) +
#   labs(x = "z-Shannon Evenness weighted by spp", y = "Species per observation", title = "Insect Orders")
# 
# 
# ## Class: shannonE_z weighted by obs vs species per obs
# 
# class_obs <- ggplot(shannon_e_expected, aes(x = shannonE_class_z_obs, y = spp_per_obs)) +
#   geom_point(alpha = 0.1) + geom_smooth(method = "lm", se = F) +  ylim(0,1) +
#   labs(x = "z-Shannon Evenness weighted by obs", y = "Species per observation", title = "All Classes")
# 
# 
# ## Class: shannonE_z weighted by spp vs species per obs
# 
# class_spp <- ggplot(shannon_e_expected, aes(x = shannonE_class_z_spp, y = spp_per_obs)) +
#   geom_point(alpha = 0.1) + geom_smooth(method = "lm", se = F) +  ylim(0,1) +
#   labs(x = "z-Shannon Evenness weighted byspp", y = "Species per observation", title = "All Classes")
# 
# plot_grid(class_obs, class_spp, insect_obs, insect_spp, nrow = 2)
# 
# ggsave("figs/inaturalist/shannon_e_expected_multipanel.pdf", units = "in", height = 10, width = 12)
# 
