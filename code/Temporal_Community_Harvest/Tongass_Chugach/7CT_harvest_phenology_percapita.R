##Tongass & Chugach -- community average harvest phenology -- percapita of harvest

##Harvest Phenology
##Code to create harvest distributions for species based on harvest window 

library(tidyverse)
library(readxl)
library(lubridate)
library(ggridges)

source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
rm(list = ls()[!ls() %in% c("df_temp_avg")])

df_comm_avg <- df_temp_avg

hp <- read_excel("data/harvest_taxa_phenology_99percent_harvest.xlsx", sheet = 1)
##note: need to add harvest phenology for ~5 species that are now part of 99% w/ Chugach added

##select only species that make up 99% of harvest
df <- df_comm_avg %>%
  filter(Lowest_Common_Taxon_Name %in% hp$Lowest_Common_Taxon_Name)

df_hp <- left_join(df, hp, by = "Lowest_Common_Taxon_Name") %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop, Harvest_Start, Harvest_End, Avg_Peak_Harvest_1, Avg_Peak_Harvest_2, Peak_2_Fraction_of_Peak_1)

#######HARVEST DISTRIBUTION CALCULATIONS ####

##Need to generate harvest distributions separately depending on nature of harvest
##a) Harvest distribution within year, 1 peak, where peak is within year
##b) Harvest distribution within year, 1 peak, where peak is across year
##c) Harvest distribution within year, 2 peaks
##d) Harvest distribution across year (i.e., period of spring/summer w/ no harvest), 1 peak
##e) Harvest distribution across year (i.e., period of spring/summer w/ no harvest), 2 peaks

####1) Set up dataframes for differences cases
##Convert dates to proper date format

df_hp$Harvest_Start <- as.Date(df_hp$Harvest_Start)
df_hp$Harvest_Start_j <- as.numeric(format(df_hp$Harvest_Start, "%j"))

df_hp$Harvest_End <- as.Date(df_hp$Harvest_End)
df_hp$Harvest_End_j <- as.numeric(format(df_hp$Harvest_End, "%j"))

df_hp$Avg_Peak_Harvest_1 <- as.Date(df_hp$Avg_Peak_Harvest_1)
df_hp$Avg_Peak_Harvest_1_j <- as.numeric(format(df_hp$Avg_Peak_Harvest_1, "%j"))

df_hp$Avg_Peak_Harvest_2 <- as.Date(df_hp$Avg_Peak_Harvest_2)
df_hp$Avg_Peak_Harvest_2_j <- as.numeric(format(df_hp$Avg_Peak_Harvest_2, "%j"))

str(df_hp)
#Calculate the duration of the harvest season for each species
df_hp <- df_hp %>%
  mutate(harvest_duration = as.numeric(Harvest_End_j - Harvest_Start_j))

##Dataframe formation 
##a) Harvest distribution within year, 1 peak, where peak is within year
df_hp_w_1 <- df_hp %>%
  filter(is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration > 0)  %>%
  filter(!Lowest_Common_Taxon_Name %in% c("Chiton", "Clam", "Cockle", "Sea Cucumber", "Flounder", "Sole")) ##these species the peak is across the year 
##b) Harvest distribution within year, 1 peak, where peak is crosses yearly border (wraps)
df_hp_w_1_b <- df_hp %>%
  filter(is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration > 0)  %>%
  filter(Lowest_Common_Taxon_Name %in% c("Chiton", "Clam", "Cockle", "Sea Cucumber", "Flounder", "Sole"))

##c) Harvest distribution within year, 2 peaks, both peaks within year
df_hp_w_2 <- df_hp %>%
  filter(!is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration > 0) 

##d) Harvest Distributions -- across year, 1 peak
df_hp_a_1 <- df_hp %>%
  filter(is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration < 0) 

##e) Harvest Distributions -- across year, 2 peak -- black bear
df_hp_a_2_1 <- df_hp %>%
  filter(!is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration < 0)%>%
  filter(Lowest_Common_Taxon_Name %in% c("Black Bear", "Brown Bear"))

##e) Harvest Distributions -- across year, 2 peak - upland game birds
df_hp_a_2_2 <- df_hp %>%
  filter(!is.na(Avg_Peak_Harvest_2)) %>%
  filter(harvest_duration < 0) %>%
  filter(Lowest_Common_Taxon_Name == "Upland Game Birds")


##2a) Harvest Distributions -- within year, 1 peak, and full distribution within year -----------------
distribute_harvest <- function(amount, duration, peak_date) {
  # Calculate weights for distribution
  weights <- dnorm(1:duration, mean = peak_date, sd = 0.1 * duration) # Adjust the standard deviation as needed
  # Scale weights to sum to the total amount
  weights <- amount * weights / sum(weights)
  return(weights)
}

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_w_1), function(i) {
  amount <- df_hp_w_1$Percapita_Pounds_Harvested_sum[i]
  duration <- df_hp_w_1$harvest_duration[i]
  peak_date <- as.numeric(df_hp_w_1$Avg_Peak_Harvest_1_j[i] - df_hp_w_1$Harvest_Start_j[i])
  Site <- df_hp_w_1$Site_Year_Code[i]
  Species <- df_hp_w_1$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_w_1$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date)
  # Create a dataframe with date and harvest amount
  data.frame(date = seq(df_hp_w_1$Harvest_Start_j[i], by = 1, length.out = duration),
             harvest_amount = harvest_amount, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_w_1 <- do.call(rbind, simulated_harvest_prop) 

sp_dist_plot <- simulated_harvest_prop_df_w_1 %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species, scale = "free")
sp_dist_plot


##2b) Harvest Distributions -- within year, 1 peak, but peak crosses yearly border (wraps) -------------
distribute_harvest <- function(amount, duration, peak_date, date_order) {
  # Generate sequence of days (1 to duration)
  days <- 1:duration
  
  # Calculate weights for distribution with circular consideration
  circular_days <- ((days + peak_date) - (-(duration/2))) %% duration
  circular_days <- as.data.frame(circular_days) %>%
    mutate(date_order = 1:365)
  
  weights <- dnorm(circular_days$date_order, mean = 182.5, sd = 0.1 * duration)
  
  # Scale weights to sum to the total amount
  weights <- amount * weights / sum(weights)
  
  result <- data.frame(circular_days = circular_days$circular_days, weights = weights)
  
  return(result)
}

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_w_1_b), function(i) {
  amount <- df_hp_w_1_b$Percapita_Pounds_Harvested_sum[i]
  duration <- df_hp_w_1_b$harvest_duration[i]
  peak_date <- as.numeric(df_hp_w_1_b$Avg_Peak_Harvest_1_j[i])
  Site <- df_hp_w_1_b$Site_Year_Code[i]
  Species <- df_hp_w_1_b$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_w_1_b$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date, date_order)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$circular_days,
             harvest_amount = harvest_amount$weights, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_w_1_b <- do.call(rbind, simulated_harvest_prop)%>%
  mutate(date = date + 0.5)


sp_dist_plot_2 <- simulated_harvest_prop_df_w_1_b %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#003366","#CC9966"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_2

##2c) Harvest Distributions -- within year, 2 peaks(first peak higher) -----------------
df_hp_w_2_a <- df_hp_w_2 %>%
  filter(Lowest_Common_Taxon_Name == "Rainbow Trout")
distribute_harvest_bimodal <- function(amount, duration, peak_date1, peak_date2, peak_ratio) {
  days <- 1:duration
  # Calculate weights for first peak for distribution with circular consideration
  circular_days_1 <- ((days + peak_date1) - (-(duration/2))) %% duration
  circular_days_1 <- as.data.frame(circular_days_1) %>%
    mutate(date_order = 1:365)
  
  weights1 <- dnorm(circular_days_1$date_order, mean = 182.5, sd = 0.1 * duration)
  # Calculate weights for second peak for distribution with circular consideration
  circular_days_2 <- ((days + peak_date2) - (-(duration/2))) %% duration
  circular_days_2 <- as.data.frame(circular_days_2) %>%
    mutate(date_order = 1:365)
  
  weights2 <- dnorm(circular_days_2$date_order, mean = 182.5, sd = 0.1 * duration)
  
  # Scale weights to sum to the total amount and adjust peak ratio
  weights1 <- amount * weights1 * 1 / sum(weights1)
  weights2 <- amount * weights2 * (1-0.5) / sum(weights2)
  
  weights1_df <- cbind(circular_days_1, weights1) %>%
    rename(circular_days = "circular_days_1")
  weights2_df <- cbind(circular_days_2, weights2)%>%
    rename(circular_days = "circular_days_2")
  # Combine weights from both peaks
  weights <- left_join(weights1_df, weights2_df, by = "circular_days") %>%
    select(circular_days, weights1, weights2) %>%
    mutate(weights = weights1 + weights2)
  
  return(weights)
}
###Need to check over the math of this, to make sure this makes sense... 

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_w_2_a), function(i) {
  amount <- df_hp_w_2_a$Percapita_Pounds_Harvested_sum[i]
  duration <- df_hp_w_2_a$harvest_duration[i]
  peak_date1 <- as.numeric(df_hp_w_2_a$Avg_Peak_Harvest_1_j[i])
  peak_date2 <- as.numeric(df_hp_w_2_a$Avg_Peak_Harvest_2_j[i])
  peak_ratio <- df_hp_w_2_a$Peak_2_Fraction_of_Peak_1[i]
  Site <- df_hp_w_2_a$Site_Year_Code[i]
  Species <- df_hp_w_2_a$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_w_2_a$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest_bimodal(amount, duration, peak_date1, peak_date2, peak_ratio)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$circular_days,
             harvest_amount = harvest_amount$weights, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_w_2_a <- do.call(rbind, simulated_harvest_prop) %>%
  mutate(date = date + 0.5)

sp_dist_plot_4 <- simulated_harvest_prop_df_w_2_a %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_4


##2c) Harvest Distributions -- within year, 2 peaks(second peak higher) -----------------
df_hp_w_2_b <- df_hp_w_2 %>%
  filter(Lowest_Common_Taxon_Name != "Rainbow Trout")
distribute_harvest_bimodal <- function(amount, duration, peak_date1, peak_date2, peak_ratio) {
  days <- 1:duration
  # Calculate weights for first peak for distribution with circular consideration
  circular_days_1 <- ((days + peak_date1) - (-(duration/2))) %% duration
  circular_days_1 <- as.data.frame(circular_days_1) %>%
    mutate(date_order = 1:365)
  
  weights1 <- dnorm(circular_days_1$date_order, mean = 182.5, sd = 0.1 * duration)
  # Calculate weights for second peak for distribution with circular consideration
  circular_days_2 <- ((days + peak_date2) - (-(duration/2))) %% duration
  circular_days_2 <- as.data.frame(circular_days_2) %>%
    mutate(date_order = 1:365)
  
  weights2 <- dnorm(circular_days_2$date_order, mean = 182.5, sd = 0.1 * duration)
  
  # Scale weights to sum to the total amount and adjust peak ratio
  weights1 <- amount * weights1 * (1-0.5) / sum(weights1)
  weights2 <- amount * weights2 * 1 / sum(weights2)
  
  weights1_df <- cbind(circular_days_1, weights1) %>%
    rename(circular_days = "circular_days_1")
  weights2_df <- cbind(circular_days_2, weights2)%>%
    rename(circular_days = "circular_days_2")
  # Combine weights from both peaks
  weights <- left_join(weights1_df, weights2_df, by = "circular_days") %>%
    select(circular_days, weights1, weights2) %>%
    mutate(weights = weights1 + weights2)
  
  return(weights)
}
###Need to check over the math of this, to make sure this makes sense... 

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_w_2_b), function(i) {
  amount <- df_hp_w_2_b$Percapita_Pounds_Harvested_sum[i]
  duration <- df_hp_w_2_b$harvest_duration[i]
  peak_date1 <- as.numeric(df_hp_w_2_b$Avg_Peak_Harvest_1_j[i])
  peak_date2 <- as.numeric(df_hp_w_2_b$Avg_Peak_Harvest_2_j[i])
  peak_ratio <- df_hp_w_2_b$Peak_2_Fraction_of_Peak_1[i]
  Site <- df_hp_w_2_b$Site_Year_Code[i]
  Species <- df_hp_w_2_b$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_w_2_b$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest_bimodal(amount, duration, peak_date1, peak_date2, peak_ratio)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$circular_days,
             harvest_amount = harvest_amount$weights, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_w_2_b <- do.call(rbind, simulated_harvest_prop) %>%
  mutate(date = date + 0.5)

sp_dist_plot_5 <- simulated_harvest_prop_df_w_2_b %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#003366"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_5

##2d) Harvest Distributions -- across year, 1 peak -----------------
distribute_harvest <- function(amount, duration, peak_date, days) {
  # Calculate weights for distribution
  weights <- dnorm(1:duration, mean = peak_date, sd = 0.1 * duration) # Adjust the standard deviation as needed
  # Scale weights to sum to the total amount
  weights <- amount * weights / sum(weights)
  weights <- cbind(days, weights)
  
  yr_date <- as.data.frame(1:365) %>%
    rename(date = "1:365")
  
  results <- left_join(yr_date, weights, by = "date")
  
  return(results)
}

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_a_1), function(i) {
  amount <- df_hp_a_1$Percapita_Pounds_Harvested_sum[i]
  duration <- (365-df_hp_a_1$Harvest_Start_j[i]) + (1+ df_hp_a_1$Harvest_End_j[i])
  peak_date <- as.numeric(df_hp_a_1$Avg_Peak_Harvest_1_j[i] - df_hp_a_1$Harvest_Start_j[i])
  days <- data.frame(c(seq(245, 365), seq(1, 59))) ##are the start and end dates respectively
  names(days)[1] <- "date"
  Site <- df_hp_a_1$Site_Year_Code[i]
  Species <- df_hp_a_1$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_a_1$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date, days)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$date,
             harvest_amount = harvest_amount$weights, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_a_1 <- do.call(rbind, simulated_harvest_prop) 

sp_dist_plot_6 <- simulated_harvest_prop_df_a_1 %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#339933"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_6

##2e) Harvest Distributions -- across year, 2 peaks -- black bear  -----------------
distribute_harvest_bimodal <- function(amount, duration, peak_date1, peak_date2, days) {
  weights1 <- dnorm(days, mean = peak_date1, sd = 0.1*duration) 
  weights2 <- dnorm(days, mean = peak_date2, sd = 0.1*duration) 
  
  # Scale weights to sum to the total amount and adjust peak ratio
  weights1 <- amount * weights1 * (1-0.75) / sum(weights1)
  weights2 <- amount * weights2 * 1 / sum(weights2)
  
  days_2 <- data.frame(c(seq(245, 365), seq(1, 182))) 
  names(days_2)[1] <- "date"
  
  weights <- cbind(days_2, weights1, weights2) %>%
    mutate(weight = weights1 + weights2)
  
  
  yr_date <- as.data.frame(1:365) %>%
    rename(date = "1:365")
  
  results <- left_join(yr_date, weights, by = "date")
  return(results)
}
###Need to check over the math of this, to make sure this makes sense... 

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_a_2_1), function(i) {
  amount <- df_hp_a_2_1$Percapita_Pounds_Harvested_sum[i]
  duration <- (365-df_hp_a_2_1$Harvest_Start_j[i]) + (1+df_hp_a_2_1$Harvest_End_j[i])
  days <- 1:duration
  peak_date1 <- df_hp_a_2_1$Avg_Peak_Harvest_2_j[i] - df_hp_a_2_1$Harvest_Start_j[i]
  peak_date2 <- (365-df_hp_a_2_1$Harvest_Start_j[i]) + df_hp_a_2_1$Avg_Peak_Harvest_1_j[i]
  
  
  Site <- df_hp_a_2_1$Site_Year_Code[i]
  Species <- df_hp_a_2_1$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_a_2_1$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest_bimodal(amount, duration, peak_date1, peak_date2, days)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$date,
             harvest_amount = harvest_amount$weight, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_a_2_1 <- do.call(rbind, simulated_harvest_prop) 

sp_dist_plot_7 <- simulated_harvest_prop_df_a_2_1 %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#339933"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_7


##2f) Harvest Distributions -- across year, 2 peaks -- upland game birds  -----------------

distribute_harvest_bimodal <- function(amount, duration, peak_date1, peak_date2, days) {
  weights1 <- dnorm(days, mean = peak_date1, sd = 0.1*duration) 
  weights2 <- dnorm(days, mean = peak_date2, sd = 0.1*duration) 
  
  # Scale weights to sum to the total amount and adjust peak ratio
  weights1 <- amount * weights1 * 1 / sum(weights1)
  weights2 <- amount * weights2 * 1 / sum(weights2)
  
  days_2 <- data.frame(c(seq(214, 365), seq(1, 136))) 
  names(days_2)[1] <- "date"
  
  weights <- cbind(days_2, weights1, weights2) %>%
    mutate(weight = weights1 + weights2)
  
  
  yr_date <- as.data.frame(1:365) %>%
    rename(date = "1:365")
  
  results <- left_join(yr_date, weights, by = "date")
  return(results)
}
###Need to check over the math of this, to make sure this makes sense... 

# Apply the function to each row in the dataframe
##seeing how looks if do proportion of harvest
simulated_harvest_prop <- lapply(1:nrow(df_hp_a_2_2), function(i) {
  amount <- df_hp_a_2_2$Percapita_Pounds_Harvested_sum[i]
  duration <- (365-df_hp_a_2_2$Harvest_Start_j[i]) + (1+df_hp_a_2_2$Harvest_End_j[i])
  days <- 1:duration
  peak_date1 <- df_hp_a_2_2$Avg_Peak_Harvest_2_j[i] - df_hp_a_2_2$Harvest_Start_j[i]
  peak_date2 <- (365-df_hp_a_2_2$Harvest_Start_j[i]) + df_hp_a_2_2$Avg_Peak_Harvest_1_j[i]
  
  
  Site <- df_hp_a_2_2$Site_Year_Code[i]
  Species <- df_hp_a_2_2$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp_a_2_2$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest_bimodal(amount, duration, peak_date1, peak_date2, days)
  # Create a dataframe with date and harvest amount
  data.frame(date = harvest_amount$date,
             harvest_amount = harvest_amount$weight, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_prop_df_a_2_2 <- do.call(rbind, simulated_harvest_prop) 

sp_dist_plot_8 <- simulated_harvest_prop_df_a_2_2 %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#339933"))+
  labs(x = "Date", y = "Percapita Harvest Amount", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)
sp_dist_plot_8




##Combine all simulated distribution dataframes ---------------
simulated_harvest_prop_df <- rbind(simulated_harvest_prop_df_w_1, simulated_harvest_prop_df_w_1_b, simulated_harvest_prop_df_w_2_a, simulated_harvest_prop_df_w_2_b, simulated_harvest_prop_df_a_1, simulated_harvest_prop_df_a_2_1, simulated_harvest_prop_df_a_2_2)

write.csv(simulated_harvest_prop_df, "data/intermediate_data/temporal_simulated_harvest_distributions_harvest_percapita.csv")

##Plot all species 
sp_dist_plot_all <- simulated_harvest_prop_df %>%
  ungroup() %>%
  group_by(species, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(total_harvest_amount = sum)) %>%
  ggplot(aes(x = date, y = total_harvest_amount, color = habitat)) +
  #geom_point() +
  geom_path() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (%)", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species, scale = "free")
sp_dist_plot_all


