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

## ggplot theme
theme_set(theme_classic(base_size = 18))

# #### Figure 1: iNat user and observation growth over time ####
# 
# annual_growth <- read.csv("data/inat_thru_2019_annual_growth.csv", stringsAsFactors = F)
# 
# #### Figure 2: spatial and temporal biases ####
# 
# ## 2a) Map of observations by country
# 
# data(wrld_simpl)
# world <-  wrld_simpl %>% 
#   st_as_sf()
# 
# # Data file of iNat observation coordinates is on Grace's google drive
# 
# ## 2b) land cover
# 
# nlcd_palette <- c("Open Water" = "#788cbe", "Developed Open Space" = "#dec9c9", "Developed Low Intensity" = "#fde5e4",
#                   "Developed Medium Intensity" = "#f7b29f", "Developed High Intensity" = "#e7564e",
#                   "Barren Land (Rocky/Sand/Clay)" = "#b3ada3",
#                   "Deciduous Forest" = "#69ab63", "Evergreen Forest" = "#1c6330", "Mixed Forest" = "#b5c98f",
#                   "Shrub/Scrub" = "#ccba7d", "Grassland/Herbaceous" = "#e3e3c2", "Pasture/Hay" = "#dbd93d",
#                   "Cultivated Crops" = "#ab7029", "Woody Wetlands" = "#bad9eb", "Emergent Herbaceous Wetlands" = "#70a3ba")
# 
# nlcd_legend <- read.csv("data/nlcd2016_legend.csv", stringsAsFactors = F)
# 
# # Land cover for iNat observation sites, 0's and NA are no data
# inat_landcover <- read.csv("data/inat_site_landcover.csv", stringsAsFactors = F)
# 
# # Land cover distribution for whole US
# #https://www.mrlc.gov/data/statistics/national-land-cover-database-2016-nlcd2016-statistics-2016
# 
# ## 2c) species per class
# 
# inat_species <- read.csv("data/inat_taxo.csv", stringsAsFactors = F)
# 
# eol_spp_per_class <- read.csv("data/species_cnts_by_taxon.csv", stringsAsFactors = F)
# 
# ## 2d) 2018 observation phenology
# 
# # City nature challenge - jday 119 in 2018
# jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
# dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 
# inat_2018_pheno <- read.csv("data/inat_2018_annual_user_pheno.csv", stringsAsFactors = F)

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
  labs(x = "Observations", y = "Species")

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

# 50, 90, 99th percentiles

quantile(user_prop_obs$total_obs, c(0.5, 0.9, 0.99))
# 3, 29, 455

panel2 <- ggplot(user_prop_obs, aes(x = total_obs, y = cum_obs)) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000)) + geom_line() +
  geom_ribbon(data = filter(user_prop_obs, total_obs <= 3), aes(x = total_obs, ymin = 0, ymax = cum_obs), alpha = 0.4, fill = "skyblue") +
  geom_ribbon(data = filter(user_prop_obs, total_obs <= 29), aes(x = total_obs, ymin = 0, ymax = cum_obs), alpha = 0.4, fill = "skyblue") +
  geom_ribbon(data = filter(user_prop_obs, total_obs <= 455), aes(x = total_obs, ymin = 0, ymax = cum_obs), alpha = 0.4, fill = "skyblue") +
  geom_vline(xintercept = 3, lty = 2, col = "darkgray") +
  geom_vline(xintercept = 29, lty = 2, col = "darkgray") +
  geom_vline(xintercept = 455, lty = 2, col = "darkgray") +
  annotate(geom = "text", x = 3, y = 1, label = "50% of\nusers", size = 4) +
  annotate(geom = "text", x = 29, y = 1, label = "90% of\nusers", size = 4) +
  annotate(geom = "text", x = 455, y = 1, label = "99% of\nusers", size = 4) +
  labs(x = "Observations", y = "Proportion of observations")

plot_grid(panel1, panel2, nrow = 1, labels = c("A", "B"))

ggsave("iNatUserBehavior/figs/fig3_nSpp_vs_nObs.pdf", units = "in", width = 10, height = 5)

##### Figure 4: All classes taxonomic clustering ######

##### Figure 5: Insecta taxonomic clustering ####
