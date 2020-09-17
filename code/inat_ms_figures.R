### iNaturalist user behavior MS
## Figures and analysis

#### Libraries

library(tidyverse)
library(cowplot)

theme_set(theme_classic(base_size = 18))

#### Figure 3: Are users taxonomically specialized? ####

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

class_mod <- lm(log(spp_per_obs/(1 - spp_per_obs)) ~ shannonE_class, data = user_specializ)
order_mod <- lm(log(spp_per_obs_insect/(1 - spp_per_obs_insect)) ~ shannonE_class, data = user_specializ)

class_plot <- ggplot(user_specializ, aes(x = shannonE_class, y = spp_per_obs)) + 
  geom_point(alpha = 0.1) + geom_smooth(method = "lm", se= F) +
  labs(x = "Shannon Evenness", y = "Species per observation") + 
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25), "in"))

order_plot <- ggplot(user_specializ, aes(x = shannonE_order, y = spp_per_obs_insect)) + 
  geom_point(alpha = 0.1) + geom_smooth(method = "lm", se= F) +
  labs(x = "Shannon Evenness", y = "Species per observation") + 
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25), "in"))

plot_grid(class_plot, order_plot, nrow = 1, labels = c("A) All Classes", "B) Insect Orders"))
ggsave("figs/inaturalist/shannon_evenness_scatter.pdf", height = 5, width = 10, units = "in")

shannonE <- ggplot(user_specializ, aes(x = shannonE_class)) + 
  geom_histogram(aes(fill = "All Classes"), alpha = 0.5) + 
  geom_histogram(aes(x = shannonE_order, fill = "Insect Orders"), alpha = 0.5) +
  scale_fill_manual(values = c("All Classes" = "skyblue3", "Insect Orders" = "springgreen3")) +
  labs(x = "Shannon Evenness", y = "Count", fill = "") + 
  theme(legend.position = c(0.8, 0.9))

user_insect_all <- ggplot(user_specializ, aes(x = shannonE_class, y = shannonE_order)) +
  geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, col = "blue", lty = 2, cex = 1) +
  labs(x = "Shannon Evenness - All Classes", y = "Shannon Evenness - Insect Orders")

plot_grid(shannonE, user_insect_all, nrow = 1)
ggsave("figs/inaturalist/shannon_evenness_distributions.pdf", height = 5, width = 10, units = "in")
