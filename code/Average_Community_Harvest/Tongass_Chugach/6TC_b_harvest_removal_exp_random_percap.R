##Harvest removal experiment -- randomly removing species -- PERCAPITA HARVEST
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(purrr)
library(broom)
library(minpack.lm)

##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

df_comm_avg <- df_comm_avg %>%
  select(Site:Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg_total) %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0)

##Harvest removal experiment -- random species loss ---------------
# ##For each community/year, create rank # based on percapita harvest
# ##first for total harvest
 total_harvest_rank <- df_comm_avg %>%
   group_by(Site) %>%
   mutate(harvest_rank = rank(-Percapita_Pounds_Harvested_sum_avg, ties.method = "min")) 
# 
total_harvest_rank <- total_harvest_rank %>%
  group_by(Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())


# 
# ##Harvest Removal Experiment -- remove species randomly 
results_1000 <- data.frame(
   iteration = integer(),
   community = character(),
   species_removed = integer(),
   total_harvest = numeric(),
   stringsAsFactors = FALSE
 )
# # Function to calculate total harvest after removing a particular species
# 
 calculate_total_harvest <- function(df, species_to_remove) {
   total_harvest <- sum(df$Percapita_Pounds_Harvested_sum_avg)
   removed_harvest <- df$Percapita_Pounds_Harvested_sum_avg[df$Lowest_Common_Taxon_Name %in% species_to_remove]
   remaining_harvest <- total_harvest - sum(removed_harvest)
   return(remaining_harvest)
 }
# 
# 
# ##Iterate the experiment 1000 times
 for(iteration in 1:1000) {
   print(paste("Iteration:", iteration))

# # Iterate over each community
 communities <- unique(total_harvest_rank$Site)
#
 for (community in communities) {
   cat("Community:", community, "\n")
  
#   # Extract harvest data for the current community
   community_data <- subset(total_harvest_rank, Site == community)
   
#   # Shuffle the list of species within the community
   shuffled_species <- sample(unique(community_data$Lowest_Common_Taxon_Name))
   
   # Initialize variables to store results for the current community
   species_removed <- 0
   remaining_harvest <- sum(community_data$Percapita_Pounds_Harvested_sum_avg)
   
#   # Sequential removals of species and calculation of total harvest
   for (species in shuffled_species) {
     remaining_harvest <- calculate_total_harvest(community_data, species)
     species_removed <- species_removed + 1
     # Store results for the current community
     results_1000 <- rbind(results_1000, data.frame(iteration = iteration, community = community, species_removed = species_removed, total_harvest = remaining_harvest))
     community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
     }
   }
 }


 ##due to memory limitations, 855 simulations were successfully completed for the published reuslts

##add row for 0 species removed and 100% harvest
total_harvest_percap <- df_comm_avg %>%
  ungroup() %>%
  select(Site, Percapita_Pounds_Harvested_sum_avg_total) %>%
  unique() %>%
  rename(total_harvest = "Percapita_Pounds_Harvested_sum_avg_total", community = "Site") %>%
  mutate(species_removed = 0)


##repeat so have 0 harvest for every iteration
total_harvest_repeated <- total_harvest_percap %>%
  slice(rep(1:n(), each = 855)) %>%
  mutate(iteration = rep(1:855, times = nrow(total_harvest_percap))) %>%
  select(iteration, community, species_removed, total_harvest)


results_2 <- rbind(results_1000, total_harvest_repeated)


write.csv(results_2, "data/intermediate_data/random_harvest_loss_1000_percap.csv")

##Start here to avoid re-running simulations ----------------
results_2 <- read.csv("data/intermediate_data/random_harvest_loss_1000_percap.csv") %>%
  filter(iteration <= 855)

##extract slope and relate to species richness --------LINEAR ------------
# List to store regression results
community_iteration_list <- split(results_2, list(results_2$community, results_2$iteration))##want to split by both community and iteration 

regression_results <- list()

# Perform linear regression iteration for each community
for (community in names(community_iteration_list)) {
  community_df <- community_iteration_list[[community]]
  regression <- lm(total_harvest ~ species_removed, data = community_df)
  regression_results[[community]] <- summary(regression)
}


str(regression_results)
# Initialize an empty dataframe to store results
results_df <- data.frame(Community.iteration = character(),
                         Coefficients = character(),
                         Value = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each community's regression result
for (community in names(regression_results)) {
    summary <- regression_results[[community]]
    coefficients <- summary$coefficients 
  
 
  # Store coefficients in the dataframe
  for (i in 1:nrow(coefficients)) {
    results_df <- rbind(results_df, 
                        data.frame(Community = community,
                                   Coefficients = rownames(coefficients)[i],
                                   Value = coefficients[i, "Estimate"], stringsAsFactors = FALSE))
  }
}

##Calculate mean and SD of slope
lm_mean_slopes <- results_df %>%
  separate(Community, into = c("Community", "Iteration"), sep = "\\.", remove = FALSE) %>%
  filter(Coefficients == "species_removed") %>%
  group_by(Community) %>%
  summarise_at(vars(Value), list(slope_mean = mean, slope_sd = sd))


write.csv(lm_mean_slopes, "data/intermediate_data/1000_random_removal_slope_mean_sd.csv")

