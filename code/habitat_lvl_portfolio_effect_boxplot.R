###CV of total harvest vs. CV of each habitat -- seasonal and decadal 



library(ggplot2)
library(tidyverse)
library(ggpubr)

###SEASONAL 

simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv") %>%
  mutate(harvest_amount_kg = harvest_amount*0.45359237)

total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_total = sum))

total_simulated_harvest_cv <- total_simulated_harvest %>%
  mutate(mean_th = mean(harvest_total),
         sd_th = sd(harvest_total),
         cv_th = sd_th/mean_th) %>%
  select(site, mean_th:cv_th) %>%
  distinct()

total_harvest_habitat <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, date, habitat) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_total = sum)) 


total_harvest_habitat_cv <- total_harvest_habitat %>%
  group_by(site, habitat) %>%
  mutate(mean_h = mean(harvest_total),
       sd_h = sd(harvest_total),
       cv_h = sd_h/mean_h) %>%
  ungroup() %>%
  select(site, habitat, mean_h:cv_h) %>%
  distinct()

seasonal_mean_df_h_cv <- total_harvest_habitat_cv %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(seasonal_mean_habitat_cv = mean(cv_h)) %>%
  select(site, seasonal_mean_habitat_cv) %>% 
  distinct()



seasonal_harvest_cv <- total_simulated_harvest_cv %>%
  select(site, cv_th) %>%
  left_join(seasonal_mean_df_h_cv, by = "site") %>%
  rename(mean_habitat_cv = "seasonal_mean_habitat_cv", Community = "site") %>%
  mutate(time_type = "Seasonal")


####DECADAL -----------
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

##Calculate total harvest per habitat each year
df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  mutate(percapita_kg = Percapita_Pounds_Harvested_sum_total*0.45359237) %>%
  rename(Community = "Site")

df_h_cv <- df_h %>%
  ungroup() %>%
  group_by(Community, Habitat) %>%
  mutate(mean_h = mean(percapita_kg),
         sd_h = sd(percapita_kg),
         cv_h = sd_h/mean_h) %>%
  select(Community, mean_h:cv_h) %>%
  distinct()

mean_df_h_cv <- df_h_cv %>%
  ungroup() %>%
  group_by(Community) %>%
  mutate(mean_habitat_cv = mean(cv_h)) %>%
  select(Community, mean_habitat_cv) %>% 
  distinct()

total_harvest_cv <- df_temp_avg %>%
  group_by(Site_Year_Code, Community, Year) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(total_harvest = sum)) %>%
  ungroup() %>%
  group_by(Community) %>%
  mutate(mean_th = mean(total_harvest),
         sd_th = sd(total_harvest),
         cv_th = sd_th/mean_th) %>%
  select(Community, mean_th:cv_th) %>%
  distinct()


decadal_harvest_cv <- total_harvest_cv %>%
  select(Community, cv_th) %>%
  left_join(mean_df_h_cv, by = "Community") %>%
  mutate(time_type = "Decadal")


harvest_cv_all <- rbind(decadal_harvest_cv, seasonal_harvest_cv) %>%
  pivot_longer(cols = c("cv_th", "mean_habitat_cv"), names_to = "cv_type", values_to = "CV") %>%
  mutate(`CV Type` = case_when(
    startsWith(cv_type, "cv") ~ "Total Harvest CV",
    startsWith(cv_type, "mean") ~ "Mean Habitat CV",
  ))


##Plot boxplot 
ggplot(harvest_cv_all, aes(x = time_type, y = CV, fill = `CV Type`)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), strip.background = element_blank())


