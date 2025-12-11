##Calculate harvest proportions for each site*year


library(tidyverse)
library(readxl)

##Read in comparable harvest data 
df <- read.csv("data/TC_comparable_harvest_df.csv")

survey_demographics <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) %>%
  dplyr::rename(Forest = "National_Forest")

df_temp <- left_join(df, survey_demographics, by = c("Forest", "Site_Year_Code", "Sampled_households", "Est_Num_Community_Households", "Sampled_Population", "Est_Comm_Population")) %>%
  filter(Temporal_Survey == "Y")


##Calculate harvest proportion for each year 
##Calculate proportions, do transformations and normalize percapita harvest and total harvest 
df_temp_avg_total <- df_temp %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  group_by(Forest, Community, Year) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

df_temp_avg <- df_temp %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>% 
  left_join(df_temp_avg_total, by = c("Forest", "Community", "Year")) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Forest, Community, Year) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop))

