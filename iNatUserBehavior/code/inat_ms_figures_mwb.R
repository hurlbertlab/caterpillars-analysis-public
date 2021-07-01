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
theme_set(theme_classic(base_size = 18))

#### Building Necessary Sub-Panels for Figure 1 ###

## Annual growth Rate of iNat Use

annual_growth <- read.csv("iNatUserBehavior/data/inat_thru_2019_annual_growth.csv", stringsAsFactors = F)

annual_growth_plot <- ggplot() +
  geom_line(annual_growth, mapping = aes(x = year, y = n_users/ 100000), 
            size = 1.25, color = "Purple") +
  geom_line(annual_growth, mapping = aes(x = year, y = n_obs / 1000000), 
            size = 1.25, color = "Black") +
  labs(x = "Year", y = " Unique Users") + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))  + 
  scale_y_continuous(name = "Observations (M)", 
                     breaks = c(0, 2, 4, 6, 8, 10, 12), 
                     sec.axis = sec_axis(trans = ~ . / 10 , 
                                         name = "Users (M)", 
                                         breaks = c(0,0.2,0.4,0.6, 0.8, 1, 1.2))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y.right = element_text(color = "Purple"),
        axis.line.y.right = element_line(color = "Purple"),
        axis.ticks.y.right = element_line(color = "Purple"),
        axis.text.y.right = element_text(color = "Purple")) + 
  ggtitle("A")


## 2018 observation phenology

# City nature challenge - jday 119 in 2018
jds = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
dates = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

inat_2018_pheno <- read.csv("iNatUserBehavior/data/inat_2018_annual_user_pheno.csv", stringsAsFactors = F) %>% 
  mutate(week = 0:52)

pheno <- ggplot() + 
  geom_line(inat_2018_pheno, mapping = aes(x = week, y = n_obs / 1000), 
            stat = "identity", size = 1.25) + 
  geom_line(inat_2018_pheno, mapping = aes(x = week, y = n_users / 50), 
            stat = "identity", size = 1.25, color = "purple") + 
  labs(x = "Week of the year", y = "Observations") +
  scale_y_continuous(name = "Observations (K)", 
                     breaks = c(0, 200, 400, 600, 800),
                     sec.axis = sec_axis(trans = ~ . / 20,
                                         name = "Users (K)")) +
  theme(axis.title.y.right = element_text(color = "Purple"),
        axis.line.y.right = element_line(color = "Purple"),
        axis.ticks.y.right = element_line(color = "Purple"),
        axis.text.y.right = element_text(color = "Purple")) +
  ggtitle("B")

pheno

## Weekly observations 

dow <- read.csv("iNatUserBehavior/data/day_of_week_obs_users.csv") %>% 
  filter(n_obs > 20)

dow$weekday <- as.factor(dow$weekday)
dow$weekday <- factor(dow$weekday, 
                      levels = c(
                        "Monday",
                        "Tuesday",
                        "Wednesday",
                        "Thursday",
                        "Friday",
                        "Saturday",
                        "Sunday"
                      )) 
dow <- dow %>% 
  mutate(Weekend = case_when(weekday == "Monday" ~ "Weekday",
                             weekday == "Tuesday" ~ "Weekday",
                             weekday == "Wednesday" ~ "Weekday",
                             weekday == "Thursday" ~ "Weekday",
                             weekday == "Friday" ~ "Weekday",
                             weekday == "Saturday" ~ "Weekend",
                             weekday == "Sunday" ~ "Weekend")) %>% 
  mutate(Month = lubridate::month(observed_on, label = TRUE))

dow$Weekend <- as.factor(dow$Weekend)
dow$Weekend <- factor(dow$Weekend, 
                      levels = c(
                        "Weekend", "Weekday"
                      )) 

## Modify dataframe for weekend bias plot possibility

dow_sum <- dow %>% 
  group_by(weekday) %>% 
  summarise(totalObs = sum(n_obs),
            totalUsers = sum(n_users))


dow_sum2 <- data.frame(weekday = c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun",
                                   "Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun"),
                       count = c(dow_sum$totalObs, dow_sum$totalUsers),
                       type = c(rep("Observations", 7), rep("Users", 7))
)

dow_sum2$weekday <- factor(dow_sum2$weekday, 
                           levels = c(
                             "Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun"
                           )) 

dow_sum2 <- dow_sum2 %>% 
  mutate(count2 = ifelse(type == "Users", 
                         yes = count * 3, 
                         no = count))

weekend_plot2 <- ggplot() +
  geom_bar(dow_sum2, mapping = aes(x = weekday, y = count2/1000000, fill = type), 
           stat = "identity",
           position = "dodge")+
  scale_y_continuous(name = "Observations (M)", 
                     labels=scales::comma_format(.1),
                     breaks = c(0.2,0.4,0.6,0.8,1,1.2),
                     sec.axis = sec_axis(trans = ~ . /3,
                                         name = "User days (M)",
                                         breaks = c(0.1,0.2,0.3,0.4),
                                         labels=scales::comma_format(.1)),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("black", "purple")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y.right = element_text(color = "Purple"),
        axis.line.y.right = element_line(color = "Purple"),
        axis.ticks.y.right = element_line(color = "Purple"),
        axis.text.y.right = element_text(color = "Purple")) + 
  labs(x = "Weekday") +
  theme(legend.position = "none") +
  ggtitle("C")

weekend_plot2

# Combine Top panel
top_panel2 <- cowplot::plot_grid(annual_growth_plot, pheno, weekend_plot2, ncol = 3)
#### spatial Map of observations ####

## Map of observations by country

obs1 <- readRDS("iNatUserBehavior/bigdata/inat_records_by_countries1.rds")
obs2 <- readRDS("iNatUserBehavior/bigdata/inat_records_by_countries2.rds")
obs3 <- readRDS("iNatUserBehavior/bigdata/inat_records_by_countries3.rds")

inat_world <- rbind(obs1, obs2, obs3) %>% 
  st_drop_geometry()

data(wrld_simpl)

world <-  wrld_simpl %>% 
  st_as_sf()

data(wrld_simpl)

world <-  wrld_simpl %>% 
  st_as_sf()

world2 <- left_join(world, inat_world) %>% 
  mutate(percapita = count/POP2005) %>% 
  mutate(perarea = count/AREA)

  
  
 global_obs <-  ggplot() + 
  geom_sf(world2, mapping = aes(fill = count)) +
  scale_fill_viridis_c(trans = "log10", breaks = c(1,1000,1000000),
                       guide = guide_colorbar(title.position = "top"),
                       labels = c(1, 1000, "1 Mill")) +
  labs(fill = "Observations") +
  theme(legend.position = c(0.1,0.45),
        legend.background = element_rect(colour = 'black',
                                         fill = 'white', 
                                         linetype = 'solid'),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12)) +
    guides(shape = guide_legend(override.aes = list(size = 0.2)),
           color = guide_legend(override.aes = list(size = 0.2))) +
  ggtitle("D")

## Land Cover Bias Plot

# Data file of iNat observation coordinates is on Grace's google drive

nlcd_palette <- c("Water" = "#788cbe", "Developed, Open Space" = "#dec9c9", "Developed, Low Intensity" = "#fde5e4",
                  "Developed, Medium Intensity" = "#f7b29f", "Developed, High Intensity" = "#e7564e",
                  "Bare Rock/Sand/Clay" = "#b3ada3",
                  "Deciduous Forest" = "#69ab63", "Evergreen Forest" = "#1c6330", "Mixed Forest" = "#b5c98f",
                  "Shrub/Scrub" = "#ccba7d", "Grasslands/Herbaceous" = "#e3e3c2", "Pasture/Hay" = "#dbd93d",
                  "Cultivated Crops" = "#ab7029", "Woody Wetlands" = "#bad9eb", "Herbaceous Wetlands" = "#70a3ba")

nlcd_legend <- read.csv("data/nlcd2016_legend.csv", stringsAsFactors = F)

# Land cover for iNat observation sites, 0's and NA are no data
inat_landcover <- read.csv("iNatUserBehavior/bigdata/inat_us_site_landcover.csv", stringsAsFactors = F)

inat_lc_group <- inat_landcover %>% 
  group_by(landcover) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(landcover)) %>% 
  filter(landcover != 0)

mlrc_prop <- read.csv("iNatUserBehavior/data/mrlc_proportions.csv")

inat_lc_group2 <- left_join(inat_lc_group, mlrc_prop, by = c("landcover" = "X")) %>% 
  dplyr::rename(true_perc = Percentage) %>% 
  mutate(obs_perc = count / sum(inat_lc_group$count)*100) %>% 
  rename("Land_Cover_Class" = 3) %>% 
  mutate(diff = true_perc - obs_perc) %>% 
  filter(landcover != 12)

inat_lc_group2[15,3] <- "Herbaceous Wetlands"

inat_lc_group2$Land_Cover_Class <- as.factor(inat_lc_group2$Land_Cover_Class)

inat_lc_group2$Land_Cover_Class <- factor(inat_lc_group2$Land_Cover_Class, 
                                          levels = c(
                                            "Developed, Open Space",
                                            "Developed, Low Intensity",
                                            "Developed, Medium Intensity",
                                            "Developed, High Intensity",
                                            "Deciduous Forest",
                                            "Evergreen Forest",
                                            "Mixed Forest",
                                            "Pasture/Hay",
                                            "Cultivated Crops",
                                            "Woody Wetlands",
                                            "Herbaceous Wetlands",
                                            "Bare Rock/Sand/Clay",
                                            "Water",
                                            "Grasslands/Herbaceous",
                                            "Shrub/Scrub"))


lc_plot <- ggplot(inat_lc_group2) + 
  geom_bar(stat = "identity",
           aes (y = Land_Cover_Class, x = obs_perc, fill = GeneralizedClass)) +
  geom_bar(stat = "identity",
           aes (y = Land_Cover_Class, x = true_perc, color = "Expected percentage"), fill = "transparent") +
  scale_color_manual(values = "Black")  +
  scale_fill_manual(values = c("darkorange3", "grey", "orangered3", "chartreuse4",
                               "yellow", "tan", "dodger blue", "light blue", "Pink"),
                    name = "Generalized class") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "Percentage of observations", y = "") + 
  labs(color = "") +
  ggtitle("E")

# Land cover distribution for whole US
#https://www.mrlc.gov/data/statistics/national-land-cover-database-2016-nlcd2016-statistics-2016

## Grid Arrange these all together for FIg 1 ##

lay <- rbind(c(1,1,1,1),
             c(1,1,1,1),
             c(2,2,3,3),
             c(2,2,3,3))


ga <- grid.arrange(top_panel2, global_obs, lc_plot, 
                   layout_matrix = lay, heights = c(0.9,0.9,1.25, 1.25),
                   widths = c(0.75,0.75,0.75,0.75))

ggsave(filename = "iNatUserBehavior/figs/Fig1.pdf", plot = ga,
       width = 16, height = 10, dpi = 450)

ggsave(filename = "iNatUserBehavior/figs/Fig1.png", plot = ga,
       width = 16, height = 10, dpi = 300)

### Supplemental figure - land cover distribution of all users vs. casual users ###

inat_casual_landcover <- read.csv("iNatUserBehavior/data/inat_us_site_landcover_casual.csv")

casual_lc <- inat_casual_landcover %>%
  group_by(landcover) %>%
  summarize(count = n())

lc_casual_all <- inat_lc_group2 %>%
  left_join(casual_lc, by = c("landcover"), suffix = c("_all", "_casual")) %>%
  mutate(obs_perc_casual = (count_casual/sum(count_casual))*100)

lc_casual_all[15,3] <- "Herbaceous Wetlands"
lc_casual_all[5,3] <- "Developed, High Intensity"

lc_casual_all_long <- lc_casual_all %>%
  dplyr::select(-count_all, -count_casual) %>%
  pivot_longer(c("true_perc", "obs_perc", "obs_perc_casual"), names_to = "data_source", 
               values_to = "prop_obs")
  

ggplot(lc_casual_all_long, aes(x = data_source, y = prop_obs, fill = Land_Cover_Class)) +
         geom_col(position = "stack") + coord_flip() + scale_fill_manual(values = nlcd_palette) +
  labs(x = "", y = "Percent of observations", fill = "Land cover class") +
  scale_x_discrete(labels = c("All observers",
                   "Casual observers",
                   "Expected percent"))
ggsave("iNatUserBehavior/figs/figs2_landcover_all_casual_obs.pdf", units = "in", height = 5, width = 10)

lc_casual_all %>% group_by(GeneralizedClass) %>% summarize(total_perc = sum(obs_perc_casual))
# 58% developed

lc_casual_all %>% group_by(GeneralizedClass) %>% summarize(total_perc = sum(obs_perc))
# 38% developed

lc_casual_all %>% group_by(GeneralizedClass) %>% summarize(total_perc = sum(true_perc))

### Figure 2 - Number of Species Observed Per Class ###

## 2c) species per class

inat_2019_species <- read.csv("iNatUserBehavior/data/inat_taxon_info_thru2019.csv", stringsAsFactors = F)

eol_spp_per_class <- read.csv("iNatUserBehavior/data/species_cnts_by_taxon.csv", stringsAsFactors = F)

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
  mutate(propObs = count/class_cnt) %>% 
  filter(class != "Not assigned")


obs_by_class <- ggplot(tdf2) + 
  geom_line(mapping = aes(class_cnt, y = 1), linetype = "dashed") +
  geom_point(mapping = aes(class_cnt, propObs, color = kingdom)) +
  geom_text_repel(filter(tdf2, class == "Aves"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Aves") +
  geom_text_repel(filter(tdf2, class == "Ginkgoopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Ginkgoopsida", nudge_y = -0.03) +
  geom_text_repel(filter(tdf2, class == "Insecta"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Insecta") +
  geom_text_repel(filter(tdf2, class == "Merostomata"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Merostomata", nudge_y = 0.03) +
  geom_text_repel(filter(tdf2, class == "Pinopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Pinopsida") +
  geom_text_repel(filter(tdf2, class == "Reptilia"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Reptilia", nudge_x = 0.03) +
  geom_text_repel(filter(tdf2, class == "Mammalia"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Mammalia", nudge_x = 0.03) +
  geom_text_repel(filter(tdf2, class == "Sarcopterygii"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Sarcopterygii", nudge_y = -0.03) +
  geom_text_repel(filter(tdf2, class == "Amphibia"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Amphibia", nudge_y = -0.03) +
  geom_text_repel(filter(tdf2, class == "Cycadopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Cycadopsida", nudge_x = -0.05, nudge_y = -0.05) +
  geom_text_repel(filter(tdf2, class == "Magnoliopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Magnoliopsida", nudge_y = 0.03) +
  geom_text_repel(filter(tdf2, class == "Liliopsida"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Liliopsida", nudge_y = -0.03) +
  geom_text_repel(filter(tdf2, class == "Gastropoda"), 
                  mapping = aes(class_cnt, propObs), 
                  label = "Gastropoda", nudge_x = 0.03) +
  geom_text_repel(filter(tdf2, class == "Coniocybomycetes"), 
                mapping = aes(class_cnt, propObs), 
                label = "Coniocybomycetes", nudge_y = -0.03) +
  scale_x_log10(breaks = c(1,10,100,1000,10000,100000),
                labels=scales::comma_format(accuracy = 1)) + 
  labs(x = "Named species per class", 
       y = "Proportion of species observed per class", 
       color = "Phylum") + 
  scale_color_manual(values = c("#33638DFF", "hotpink2", "#73D055FF"))

obs_by_class

ggsave(filename = "iNatUserBehavior/figs/Fig2.pdf", plot = obs_by_class, width = 8, height = 6)
ggsave(filename = "iNatUserBehavior/figs/Fig2.png", plot = obs_by_class, width = 8, height = 6)

