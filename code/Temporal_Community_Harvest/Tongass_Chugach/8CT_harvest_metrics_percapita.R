##Calculate harvest diversity metrics -- based on PERCAPITA harvest - using year level 

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tibble)

##source code that calculates average harvest across time for each community
source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
rm(list = ls()[!ls() %in% c("df_temp_avg")])

df_comm_avg <- df_temp_avg

##calculate richness, sw diversity and evenness of harvest taxa
richness <- df_comm_avg %>%
  ungroup() %>%
  select(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
  group_by(Forest, Site_Year_Code) %>%
  count() %>%
  rename(richness = "n")

df_comm_wide <- df_comm_avg %>%
  ungroup() %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum) %>%
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Pounds_Harvested_sum)

df_comm_wide[is.na(df_comm_wide)] <- 0
df_comm_wide <- df_comm_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_comm_wide <- df_comm_wide %>%
  dplyr:: select(Abalone:Wolffish)


##SW diversity 
sw_div <- as.data.frame(diversity(df_comm_wide, index = "shannon"))
sw_div <- rownames_to_column(sw_div, "Site_Year_Code") %>%
  rename(sw_diversity = `diversity(df_comm_wide, index = "shannon")`)

head(sw_div)

comm_div <- left_join(richness, sw_div, by = "Site_Year_Code") 

##evenness
comm_div <- comm_div %>%
  mutate(evenness = sw_diversity/log(richness))



##calculate harvest habitat coupling metrics (diversity, evenness, SD) -----------
df_h <- df_comm_avg %>%
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum))

##evenness based on percapita harvest (##habitat diversity/coupling values are the same for total harvest and prop harvest, so doesn't matter)
df_h_prop <- df_h %>%
  ungroup() %>%
  dplyr::select(Site_Year_Code, Habitat, Percapita_Pounds_Harvested_sum_total) %>%
  spread(key = Habitat, value = Percapita_Pounds_Harvested_sum_total)

df_h_prop[is.na(df_h_prop)] <- 0
df_h_prop <- df_h_prop %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_h_prop <- df_h_prop %>%
  dplyr:: select(Freshwater_Anadromous:Terrestrial)


##SW diversity 
sw_div_prop <- as.data.frame(diversity(df_h_prop, index = "shannon"))
sw_div_prop <- rownames_to_column(sw_div_prop, "Site_Year_Code") %>%
  rename(sw_diversity = `diversity(df_h_prop, index = "shannon")`)

h_div_prop <- sw_div_prop %>%
  mutate(richness = 4) %>%
  mutate(evenness = sw_diversity/log(richness)) 

hist(h_div_prop$evenness)



##mean and sd of harvest proportion (calcualted from per capita harvest) across habitats
df_h_total <- df_h %>%
  select(Site_Year_Code, Habitat, Percapita_Pounds_Harvested_sum_total) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum_total), list(total_percap_harvest = sum)) 

df_h_2 <- left_join(df_h, df_h_total, by = "Site_Year_Code") %>%
  mutate(prop_harvest_by_habitat = (Percapita_Pounds_Harvested_sum_total/total_percap_harvest)) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(prop_harvest_by_habitat), list(avg = mean, sd = sd)) 


##combining mean, sd and diversity

library(car)
h_df_all <- left_join(df_h_2, h_div_prop, by = "Site_Year_Code") %>%
  mutate(evenness_log = log(evenness)) %>%
  mutate(sd_log = log(sd)) %>%
  mutate(log_1_SD = log(1/sd)) %>%
  mutate(sd_0_1 = 1-(sd/0.5)) %>%
  mutate(logit_sd_0_1 = logit(sd_0_1)) ##logit transformation appropriate for proportions and %'s 

hist(h_df_all$sd)
hist(h_df_all$sd_log)
hist(h_df_all$log_1_SD)
hist(h_df_all$sd_0_1)

hist(h_df_all$logit_sd_0_1)

h_df_all <- h_df_all %>%
  dplyr::rename(sw_div_h = "sw_diversity", richness_h = "richness", evenness_h = "evenness", evenness_h_log = "evenness_log")



##Join habitat coupling metrics with harvest diversity metrics ------------
comm_div_all <- left_join(comm_div, h_df_all, by = "Site_Year_Code")


##save csv
write.csv(comm_div_all, "data/intermediate_data/temporal_harvest_diversity_metrics_percap.csv")


