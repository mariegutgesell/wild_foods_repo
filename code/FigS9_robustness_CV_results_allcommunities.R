##Harvest Robustness and seasonal CV all communities fig

library(tidyverse)
library(ggplot2)
library(ggpubr)

##Read in results from robustness and seasonal CV data 
r_param <- read.csv( "data/intermediate_data/average_harvest_removal_results_percap.csv") %>%
  select(Site, alpha) %>%
  rename(community = "Site")
r_raw <- read.csv("data/intermediate_data/species_removal_results_raw_percap.csv")
r_df <- left_join(r_raw, r_param, by = "community")

r_raw_1000 <- read.csv("data/intermediate_data/random_harvest_loss_1000_percap.csv") %>%
  filter(iteration <= 854)

r_raw_1000_slopes <- read.csv("data/intermediate_data/1000_random_removal_slope_mean_sd.csv") %>%
  rename(community = "Community")

r_raw_1000_summary <- r_raw_1000 %>%
  group_by(community, species_removed) %>%
  summarise_at(vars(total_harvest), list(total_harvest_mean = mean, total_harvest_sd = sd))

r_df <- left_join(r_raw, r_param, by = "community") %>%
  left_join(r_raw_1000_summary, by = c("community", "species_removed")) %>%
  left_join(r_raw_1000_slopes, by = "community")
#Plot r
r_fig <- ggplot(r_df, aes(x = species_removed, y = total_harvest, group = community)) +
  #  geom_point()+
  geom_smooth(aes(color = alpha), method = "loess", se = FALSE) +
  scale_color_gradient2(low = "deepskyblue",mid = "darkgoldenrod1", high = "brown3", midpoint = 0.3 ) +
  theme_classic() +
  ylab("Per capita Harvest Remaining\n(kg/person)") +
  xlab("Number of Species Removed") +
  scale_y_log10()+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "right")

r_fig


##Plot mean results from 1000. iterations 
r_fig_1000 <- ggplot(r_df, aes(x = species_removed, y = total_harvest_mean, group = community)) +
  #  geom_point()+
  geom_smooth(aes(color = slope_mean), method = "loess", se = FALSE) +
  scale_color_gradient2(high = "deepskyblue",mid = "darkgoldenrod1", low = "brown3", midpoint = -9.0 ) +
  theme_classic() +
  ylab("Mean Per capita Harvest Remaining\n(kg/person)") +
  xlab("Number of Species Removed") +
#  scale_y_log10()+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "right")

r_fig_1000


##For CV data 
cv_raw <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv") %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))
cv_param <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv")%>%
  select(site, harvest_total_cv)
cv_df <- left_join(cv_raw, cv_param, by = "site")  
  

cv_fig_2 <- ggplot(cv_df, aes(x = date, y = harvest_total, group = site)) +
  #  geom_point()+
  geom_smooth(aes(color = harvest_total_cv), method = "loess", se = FALSE) +
  scale_color_gradient2(low = "deepskyblue",mid = "darkgoldenrod1", high = "brown3", midpoint = 1.1 ) +
  theme_classic() +
  ylab("Per capita Harvest\n(kg/person)") +
  xlab("Day of Year") +
   scale_y_log10()+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "right")

cv_fig_2 
  


##interannual cv 
df_hc <- read.csv("data/intermediate_data/temporal_harvest_diversity_metrics_percap.csv") %>%
  separate(Site_Year_Code, into = c("Site", "Year"), sep = "_", remove = FALSE)

##select only sites w/ more than three years of data
df_hc <- df_hc %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site != "Valdez") 

source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
df_temp_avg <- df_temp_avg %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code)

harvest_cv <- df_temp_avg %>%
  group_by(Site_Year_Code, Community, Year) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(total_harvest = sum)) %>%
  ungroup() %>%
  group_by(Community) %>%
  mutate(mean_ph = mean(total_harvest),
         sd_ph = sd(total_harvest),
         cv_ph = sd_ph/mean_ph) %>%
 # select(Community, mean_ph:cv_ph) %>%
  distinct() %>%
  rename(Site = "Community")


ia_cv_fig_2 <- ggplot(harvest_cv, aes(x = Year, y = total_harvest, group = Site)) +
  #  geom_point()+
  geom_smooth(aes(color = cv_ph), method = "loess", se = FALSE) +
  scale_color_gradient2(low = "deepskyblue",mid = "darkgoldenrod1", high = "brown3", midpoint = 0.4 ) +
  theme_classic() +
  ylab("Per capita Harvest\n(kg/person)") +
  xlab("Year") +
  scale_y_log10()+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "right")

ia_cv_fig_2 



##Join together
cv_r_fig2 <- ggarrange(r_fig, r_fig_1000, cv_fig_2, ia_cv_fig_2, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), font.label = list(colour = "black", size = 14, family = "Avenir"))
cv_r_fig2

ggsave("figures/figure_S9.png", plot = cv_r_fig2)
