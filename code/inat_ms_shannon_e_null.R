# inaturalist descriptive MS
# Null expectations of Shannon's Evenness for users with >20 observations

library(dplyr)
library(purrr)

setwd("/proj/hurlbertlab/gdicecco/caterpillars-analysis/")

# For range of n_obs observed by users, calculate 999 shannon Evenness, z score for actual
# Weighted by # observations, weighted by # species
# 158 classes, 25 Insect orders in data

user_even_order <- read.csv("inat_user_evenness_orders.csv", stringsAsFactors = F)
user_even_class <- read.csv("inat_user_evenness_class.csv", stringsAsFactors = F)

inat_user_spp <- read.csv("inat_2019_user_spp.csv", stringsAsFactors = F)
inat_user_obs <- read.csv("inat_2019_user_obs.csv", stringsAsFactors = F)

user_specializ <- inat_user_obs %>%
  left_join(inat_user_spp) %>%
  left_join(user_even_class) %>%
  left_join(user_even_order, by = c("user.login"), suffix = c("_class", "_order")) %>%
  mutate_at(c("shannonE_order", "shannonE_class"), ~ifelse(. == 0, NA, .)) %>%
  mutate(spp_per_obs = n_spp/total_obs,
         spp_per_obs_insect = n_spp_insect/total_obs_insect) %>%
  filter(spp_per_obs <= 1, spp_per_obs_insect <= 1) %>%
  mutate_at(c("spp_per_obs", "spp_per_obs_insect"), ~ifelse(. == 1, . - 0.00001, .))

inat_species <- read.csv("inat_taxon_info_2019.csv", stringsAsFactors = F)

inat_class_obs <- inat_species %>%
  group_by(class) %>%
  summarize(obs = sum(total_obs),
            spp = n_distinct(taxon_name))

inat_order_obs <- inat_species %>%
  filter(class == "Insecta") %>%
  group_by(order) %>%
  summarize(obs = sum(total_obs),
            spp = n_distinct(taxon_name))

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
      
      shannonE <- shannonH/log(158)
      
      shannonExpected <- c(shannonExpected, shannonE)
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
      
      shannonE <- shannonH/log(158)
      
      shannonExpected <- c(shannonExpected, shannonE)
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
      
      shannonE <- shannonH/log(25)
      
      shannonExpected <- c(shannonExpected, shannonE)
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
      
      shannonE <- shannonH/log(25)
      
      shannonExpected <- c(shannonExpected, shannonE)
    }
    
    sE_mean <- mean(shannonExpected)
    sE_sd <- sd(shannonExpected)
    
    (shannonE - sE_mean)/sE_sd
    
  }))
Sys.time()

write.csv(shannon_e_expected, "/proj/hurlbertlab/gdicecco/caterpillars-analysis/inat_evenness_null_mod.csv", row.names = F)
