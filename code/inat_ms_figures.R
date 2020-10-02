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

## ggplot theme
theme_set(theme_classic(base_size = 18))

#### Figure 1: iNat user and observation growth over time ####

annual_growth <- read.csv("data/inat_thru_2019_annual_growth.csv", stringsAsFactors = F)

#### Figure 2: spatial and temporal biases ####

## 2a) Map of observations by country

data(wrld_simpl)
world <-  wrld_simpl %>% 
  st_as_sf()


## 2b) land cover

nlcd_palette <- c("Open Water" = "#788cbe", "Developed Open Space" = "#dec9c9", "Developed Low Intensity" = "#fde5e4",
                  "Developed Medium Intensity" = "#f7b29f", "Developed High Intensity" = "#e7564e",
                  "Barren Land (Rocky/Sand/Clay)" = "#b3ada3",
                  "Deciduous Forest" = "#69ab63", "Evergreen Forest" = "#1c6330", "Mixed Forest" = "#b5c98f",
                  "Shrub/Scrub" = "#ccba7d", "Grassland/Herbaceous" = "#e3e3c2", "Pasture/Hay" = "#dbd93d",
                  "Cultivated Crops" = "#ab7029", "Woody Wetlands" = "#bad9eb", "Emergent Herbaceous Wetlands" = "#70a3ba")

inat_landcover <- read.csv("data/inat_site_landcover.csv", stringsAsFactors = F)

# Land cover distribution for whole US
#https://www.mrlc.gov/data/statistics/national-land-cover-database-2016-nlcd2016-statistics-2016

## 2c) species per class

inat_2019_species <- read.csv("data/inat_2019_spp.csv", stringsAsFactors = F)

eol_spp_per_class <- read.csv("data/species_cnts_by_taxon.csv", stringsAsFactors = F)

## 2d) 2018 observation phenology

# City nature challenge - jday 119 in 2018
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

inat_2018_pheno <- read.csv("data/inat_2018_annual_user_pheno.csv", stringsAsFactors = F)

#### Figures 3-5: User behavior ####

user_even_order <- read.csv("data/inat_user_evenness_orders.csv", stringsAsFactors = F)
user_even_class <- read.csv("data/inat_user_evenness_class.csv", stringsAsFactors = F)

inat_user_spp <- read.csv("data/inat_2019_user_spp.csv", stringsAsFactors = F)
inat_user_obs <- read.csv("data/inat_2019_user_obs.csv", stringsAsFactors = F)

user_specializ <- inat_user_obs %>%
  left_join(inat_user_spp) %>%
  left_join(user_even_class) %>%
  left_join(user_even_order, by = c("user.login"), suffix = c("_class", "_order")) %>%
  mutate_at(c("shannonE_order", "shannonE_class"), ~ifelse(. == 0, NA, .)) %>%
  mutate(spp_per_obs = n_spp/total_obs,
         spp_per_obs_insect = n_spp_insect/total_obs_insect) %>%
  filter(spp_per_obs <= 1, spp_per_obs_insect <= 1) %>%
  mutate_at(c("spp_per_obs", "spp_per_obs_insect"), ~ifelse(. == 1, . - 0.00001, .))

class_mod <- lm(log(spp_per_obs/(1 - spp_per_obs)) ~ shannonE_class, data = filter(user_specializ, total_obs > 20))
order_mod <- lm(log(spp_per_obs_insect/(1 - spp_per_obs_insect)) ~ shannonE_class, data = filter(user_specializ, total_obs_insect > 20))

class_plot <- ggplot(filter(user_specializ, total_obs > 20), aes(x = shannonE_class, y = spp_per_obs)) + 
  geom_point(alpha = 0.1) + geom_smooth(method = "lm", se= F) +
  labs(x = "Shannon Evenness", y = "Species per observation") + 
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25), "in"))

order_plot <- ggplot(filter(user_specializ, total_obs_insect > 20), aes(x = shannonE_order, y = spp_per_obs_insect)) + 
  geom_point(alpha = 0.1) + geom_smooth(method = "lm", se= F) +
  labs(x = "Shannon Evenness", y = "Species per observation") + 
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25), "in"))

plot_grid(class_plot, order_plot, nrow = 1, labels = c("A) All Classes", "B) Insect Orders"))
ggsave("figs/inaturalist/shannon_evenness_scatter.pdf", height = 5, width = 10, units = "in")

shannonE <- ggplot(filter(user_specializ, total_obs > 20), aes(x = shannonE_class)) + 
  geom_histogram(aes(fill = "All Classes"), alpha = 0.5) + 
  geom_histogram(data = filter(user_specializ, total_obs_insect > 20), aes(x = shannonE_order, fill = "Insect Orders"), alpha = 0.5) +
  scale_fill_manual(values = c("All Classes" = "skyblue3", "Insect Orders" = "springgreen3")) +
  labs(x = "Shannon Evenness", y = "Count", fill = "") + 
  theme(legend.position = c(0.8, 0.9))

user_insect_all <- ggplot(filter(user_specializ, total_obs > 20, total_obs_insect > 20), aes(x = shannonE_class, y = shannonE_order)) +
  geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
  labs(x = "Shannon Evenness - All Classes", y = "Shannon Evenness - Insect Orders")

plot_grid(shannonE, user_insect_all, nrow = 1)
ggsave("figs/inaturalist/shannon_evenness_distributions.pdf", height = 5, width = 10, units = "in")

# For range of n_obs observed by users, calculate 999 shannon Evenness, z score for actual
# Weighted by # observations, weighted by # species
# 158 classes, 25 Insect orders in data

inat_species <- read.csv("data/inat_taxon_info_2019.csv", stringsAsFactors = F)

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
        group_by(class) %>%
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
        group_by(class) %>%
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
write.csv(shannon_e_expected, "data/inat_evenness_null_mod.csv", row.names = F)
