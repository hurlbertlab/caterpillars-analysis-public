### iNaturalist user behavior MS
## Figures and analysis

## Earlier iterations of plot code in inat_user_behavior.R
## Clean data for plots from iNat databases in inat_ms_data_cleaning.R
## Save figures to caterpillars-analysis-public/figs/inaturalist

#### Libraries ####

library(tidyverse)
library(cowplot)
library(sf)
library(rnaturalearth)
library(tmap)
library(maptools)
library(gridExtra)
library(ggrepel)

## ggplot theme
theme_set(theme_classic())

#### Building Necessary Sub-Panels for Figure 1 ###

## Annual growth Rate of iNat Use

annual_growth <- read.csv("data/inat_thru_2019_annual_growth.csv", stringsAsFactors = F)

obs_year <- ggplot() +
  geom_line(annual_growth, mapping = aes(x = year, y = n_obs), size = 1.25) +
  labs(x = "Year", y = "Observations") + 
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(breaks = c(0, 2500000, 5000000, 7500000,
                                10000000, 12500000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("A")

users_year <- ggplot() +
  geom_line(annual_growth, mapping = aes(x = year, y = n_users), size = 1.25) +
  labs(x = "Year", y = " Unique Users") + 
  scale_x_continuous(breaks = c(2008:2019))  + 
  scale_y_continuous(breaks = c(0,100000,200000,300000,400000,500000)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("C")

growth_cp <- plot_grid(obs_year, users_year, ncol = 1)
growth_cp


#### spatial Map of observations ####

## Map of observations by country

data(wrld_simpl)

world <-  wrld_simpl %>% 
  st_as_sf()

data(wrld_simpl)

world <-  wrld_simpl %>% 
  st_as_sf()

inat_world <- readRDS("bigdata/inat_records_perCountry.rds")

world2 <- left_join(world, inat_world) %>% 
  mutate(percapita = count/POP2005) %>% 
  mutate(perarea = count/AREA)

global_obs <- ggplot() + 
  geom_sf(world2, mapping = aes(fill = count)) +
  scale_fill_viridis_c(trans = "log10", breaks = c(1,1000,1000000),
                       guide = guide_colorbar(title.position = "top")) +
  labs(fill = "Observations") +
  theme(legend.position = "bottom") +
  ggtitle("E")


## Land Cover Bias Plot

# Data file of iNat observation coordinates is on Grace's google drive

nlcd_palette <- c("Open Water" = "#788cbe", "Developed Open Space" = "#dec9c9", "Developed Low Intensity" = "#fde5e4",
                  "Developed Medium Intensity" = "#f7b29f", "Developed High Intensity" = "#e7564e",
                  "Barren Land (Rocky/Sand/Clay)" = "#b3ada3",
                  "Deciduous Forest" = "#69ab63", "Evergreen Forest" = "#1c6330", "Mixed Forest" = "#b5c98f",
                  "Shrub/Scrub" = "#ccba7d", "Grassland/Herbaceous" = "#e3e3c2", "Pasture/Hay" = "#dbd93d",
                  "Cultivated Crops" = "#ab7029", "Woody Wetlands" = "#bad9eb", "Emergent Herbaceous Wetlands" = "#70a3ba")

nlcd_legend <- read.csv("data/nlcd2016_legend.csv", stringsAsFactors = F)

# Land cover for iNat observation sites, 0's and NA are no data
inat_landcover <- read.csv("data/inat_site_landcover.csv", stringsAsFactors = F)

inat_lc_group <- inat_landcover %>% 
  group_by(landcover) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(landcover)) %>% 
  filter(landcover != 0)

mlrc_prop <- read.csv("data/env/mrlc_proportions.csv")

inat_lc_group2 <- left_join(inat_lc_group, mlrc_prop, by = c("landcover" = "X")) %>% 
  dplyr::rename(true_perc = Percentage) %>% 
  mutate(obs_perc = count / sum(inat_lc_group$count)*100) %>% 
  rename("Land_Cover_Class" = 3) %>% 
  mutate(diff = true_perc - obs_perc)

inat_lc_group2$Land_Cover_Class <- as.factor(inat_lc_group2$Land_Cover_Class)

inat_lc_group2$Land_Cover_Class <- factor(inat_lc_group2$Land_Cover_Class, 
                                          levels = c(
                                            "Developed, Open Space",
                                            "Developed, Low Intensity",
                                            "Developed, Medium Intensity",
                                            "Developed High Intensity",
                                            "Deciduous Forest",
                                            "Evergreen Forest",
                                            "Mixed Forest",
                                            "Pasture/Hay",
                                            "Cultivated Crops",
                                            "Woody Wetlands",
                                            "Emergent Herbaceous Wetlands",
                                            "Bare Rock/Sand/Clay",
                                            "Water",
                                            "Grasslands/Herbaceous",
                                            "Shrub/Scrub"))


lc_plot <- ggplot(inat_lc_group2) + 
  geom_bar(stat = "identity",
           aes (y = Land_Cover_Class, x = obs_perc, fill = GeneralizedClass)) +
  geom_bar(stat = "identity",
           aes (y = Land_Cover_Class, x = true_perc, color = "Expected Percentage"), fill = "transparent") +
  scale_color_manual(values = "Black")  +
  scale_fill_manual(values = c("darkorange3", "grey", "orangered3", "chartreuse4",
                               "yellow", "tan", "dodger blue", "light blue"),
                    name = "Generalized Class") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "Percentage of Observations", y = "") + 
  labs(color = "") +
  ggtitle("F")

# Land cover distribution for whole US
#https://www.mrlc.gov/data/statistics/national-land-cover-database-2016-nlcd2016-statistics-2016


## 2018 observation phenology

# City nature challenge - jday 119 in 2018
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

inat_2018_pheno <- read.csv("data/inat_2018_annual_user_pheno.csv", stringsAsFactors = F)

obs_pheno <- ggplot() + 
  geom_bar(inat_2018_pheno, mapping = aes(x = jd_wk, y = n_obs), stat = "identity") + 
  labs(x = "Day of year", y = "Observations") +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("B")

users_pheno <- ggplot() + 
  geom_bar(inat_2018_pheno, mapping = aes(x = jd_wk, y = n_users), stat = "identity") + 
  labs(x = "Day of year", y = "Unique users") +
  scale_y_continuous(expand = c(0,0))  +
  ggtitle("D") 

cnc <- cowplot::plot_grid(obs_pheno, users_pheno, ncol = 1)

## Grid Arrange these all together for FIg 1 ##

lay <- rbind(c(1,2,4,4),
             c(1,2,4,4),
             c(3,3,4,4),
             c(3,3,4,4))

ga <- grid.arrange(growth_cp, cnc, global_obs, lc_plot, layout_matrix = lay, 
                   widths = c(1,1,1,1.25), heights = c(1,1,1,0.75))

ggsave(filename = "figs/Figure1.pdf", plot = ga, width = 16, height = 8)


### Figure 2 - Number of Species Observed Per Class ###

## 2c) species per class

inat_2019_species <- read.csv("data/inat_2019_spp.csv", stringsAsFactors = F)

eol_spp_per_class <- read.csv("data/species_cnts_by_taxon.csv", stringsAsFactors = F)

eol2 <- eol_spp_per_class %>% 
  filter(!is.na(class_cnt)) %>% 
  na_if("") %>% 
  fill(phylum) %>% 
  fill(kingdom)

inat_2019_per_class <- inat_2019_species %>% 
  group_by(class) %>% 
  summarise(count = n())

join_df <- left_join(inat_2019_per_class, eol2, by = "class")
join_df2 <- filter(join_df, !is.na(class_cnt))  


tdf <- left_join(eol2, inat_2019_per_class, by = "class")
tdf2 <- tdf %>% 
  mutate(count = ifelse(test = is.na(count), 
                        yes = 0,
                        no = count)) %>% 
  filter(kingdom == "Animalia" | kingdom == "Fungi" | kingdom == "Plantae") %>% 
  mutate(propObs = count/class_cnt)


obs_by_class <- ggplot(tdf2) + 
  geom_line(mapping = aes(class_cnt, y = 1), linetype = "dashed") +
  geom_point(mapping = aes(class_cnt, propObs, color = kingdom)) +
  geom_text_repel(filter(tdf2, class == "Aves"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Aves") +
  geom_text_repel(filter(tdf2, class == "Ginkgoopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Ginkgoopsida", nudge_y = -0.02, nudge_x = -0.03) +
  geom_text_repel(filter(tdf2, class == "Insecta"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Insecta") +
  geom_text_repel(filter(tdf2, class == "Merostomata"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Merostomata", nudge_y = 0.1, nudge_x = 0.3) +
  geom_text_repel(filter(tdf2, class == "Pinopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Pinopsida") +
  geom_text_repel(filter(tdf2, class == "Coniocybomycetes"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Coniocybomycetes", nudge_y = 0.05) +
  geom_text_repel(filter(tdf2, class == "Neolectales"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Neolectales") +
  geom_text_repel(filter(tdf2, class == "Cephalaspidomorphi"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Cephalaspidomorphi", nudge_y = 0.05) +
  geom_text_repel(filter(tdf2, class == "Reptilia"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Reptilia", nudge_x = 0.1) +
  geom_text_repel(filter(tdf2, class == "Mammalia"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Mammalia", nudge_y = 0.1) +
  geom_text_repel(filter(tdf2, class == "Gnetopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Gnetopsida", nudge_x = -0.2) +
  geom_text_repel(filter(tdf2, class == "Scyphozoa"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Scyphozoa", nudge_x = -0.4, nudge_y = -0.05) +
  geom_text_repel(filter(tdf2, class == "Neolectomycetes"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Neolectomycetes", nudge_x = -0.2, nudge_y = -0.05) +
  scale_x_log10() + 
  labs(x = "Log of Named Species per Class", 
       y = "Proportion of Species Observed per Class", 
       color = "Phylum") + 
  scale_color_manual(values = c("#33638DFF", "#f68f46ff", "#73D055FF"))

ggsave(filename = "figs/inat_spp_by_class_eol.pdf", plot = obs_by_class, width = 8, height = 6)



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
