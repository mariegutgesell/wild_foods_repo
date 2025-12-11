##Calculate average harvest across time for each community 

library(tidyverse)
library(vegan)
library(factoextra)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/TC_comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") ) ##remove survey as was not a full comprehensive survey, not comparable

##Calculate proportions, do transformations and normalize percapita harvest and total harvest 
df_comm_avg <- df %>%
  dplyr::select(Forest, Site_Year_Code, Habitat, Trophic_Level, Trophic_Category, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>% ##total harvest proportion and percapita harvest proportion are the same
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_") %>%
  group_by(Forest, Site, Habitat, Trophic_Level, Trophic_Category, Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(avg = mean, sd = sd)) 

df_comm_avg_total <- df_comm_avg %>%
  group_by(Site) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg), list(total = sum))

df_comm_avg <- df_comm_avg %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0) %>% ##remove anything that wasn't harvested 
  left_join(df_comm_avg_total, by = "Site") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum_avg/Estimated_Total_Pounds_Harvested_sum_avg_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum_avg/Percapita_Pounds_Harvested_sum_avg_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop))


##Get list of unique taxa across all species 
sp_list <- df_comm_avg %>%
  ungroup() %>%
  select(Lowest_Common_Taxon_Name) %>%
  unique() %>%
  filter(!grepl("Unknown", Lowest_Common_Taxon_Name))
#write.csv(sp_list, "data/TC_comparable_sp_list.csv")

