##Average Harvest by Habitat Timeseries - Seasonal and Interannual

library(tidyverse)
library(ggplot2)


##Seasonal 
simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv") %>%
  mutate(harvest_amount_kg = harvest_amount*0.45359237)


##calculate total daily harvest per habitat per day for each community, and then average that across communities 
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, date, habitat) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_daily_mean = mean, harvest_daily_sum = sum)) %>%
  ungroup() %>%
  group_by(date, habitat) %>%
  summarise_at(vars(harvest_daily_sum), list(harvest_mean = mean, harvest_sum = sum)) %>%
  mutate(harvest_mean_log = log(harvest_mean),
         harvest_sum_log = log(harvest_sum))


total_simulated_harvest_all <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_daily_mean = mean, harvest_daily_sum = sum)) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise_at(vars(harvest_daily_sum), list(harvest_mean_total = mean, harvest_sum_total = sum)) 


##Create plot 
seasonal_plot_mean_3 <- ggplot() +
#  geom_point(data = total_simulated_harvest_2, aes(x = date, y = harvest_mean, group = habitat, color = habitat)) +
  #  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_smooth(data = total_simulated_harvest, aes(x = date, y = harvest_mean, group = habitat, fill = habitat, color = habitat), alpha = 0.3) + 
  scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933", "black"))+
  scale_color_manual(values = c("#FF9999","#003366","#CC9966", "#339933", "black"))+
  #geom_smooth(data = total_simulated_harvest_all, aes(x = date, y = harvest_mean_total), color = "black", alpha = 0.3) + 
  labs(x = "Calendar Year", y = "Mean Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  # ylim(0,3.0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir"), legend.text = element_text(size = 16)) #+
#theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
seasonal_plot_mean_3

##Decadal 
ia_simulated_harvest_df <- read.csv("data/intermediate_data/temporal_simulated_harvest_distributions_harvest_percapita.csv") %>%
  separate(site, into = c("Site", "Year"), sep = "_", remove = FALSE) %>%
  mutate(harvest_amount_kg = harvest_amount*0.45359237) %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(site != "Hoonah_2016") %>%
  filter(Site != "Valdez")##removing because 3 surveys only span 3 years (want to look across decade)


ia_total_simulated_harvest <- ia_simulated_harvest_df %>% 
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(Site, Year, habitat) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_annual_mean = mean, harvest_annual_sum = sum)) %>%
  ungroup() %>%
  group_by(Year, habitat) %>%
  summarise_at(vars(harvest_annual_sum), list(harvest_mean = mean, harvest_sum = sum)) %>%
  mutate(harvest_mean_log = log(harvest_mean),
         harvest_sum_log = log(harvest_sum))


ia_total_simulated_harvest$Year <- as.numeric(ia_total_simulated_harvest$Year)
ia_plot_mean_3 <- ggplot() +
 # geom_point(data = ia_total_simulated_harvest, aes(x = Year, y = harvest_mean, group = habitat, color = habitat), alpha = 0.7) +
  #   scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_smooth(data = ia_total_simulated_harvest, aes(x = Year, y = harvest_mean, group = habitat, fill = habitat, color = habitat), alpha = 0.3) + 
  scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  scale_color_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  scale_x_continuous(breaks = seq(min(ia_total_simulated_harvest$Year), max(ia_total_simulated_harvest$Year), by = 5)) +
  labs(x = "Year", y = "Mean Annual Harvest Amount\n(kg/person)") +
  theme_classic() +
  # ylim(0,3.0) +
  theme(axis.text.x = element_text(size = 12),  axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir"), legend.text = element_text(size = 16)) #+
#theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
ia_plot_mean_3

