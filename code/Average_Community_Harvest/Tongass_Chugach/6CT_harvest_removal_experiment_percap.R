##Harvest strength distributions and removal experiment -- community avg across years -- PERCAPITA HARVEST
##Worst case scenario - species removed in order of importance
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(broom)

##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])


df_comm_avg <- df_comm_avg %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0)

##For each community/year, create rank # based on percapita  harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  group_by(Site) %>%
  mutate(harvest_rank = rank(-Percapita_Pounds_Harvested_sum_avg, ties.method = "min")) 

total_harvest_rank <- total_harvest_rank %>%
  group_by(Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())


##Harvest Removal Experiment
results <- data.frame(
  community = character(),
  species_removed = integer(),
  total_harvest = numeric(),
  stringsAsFactors = FALSE
)
# Function to calculate total harvest after removing a particular species

calculate_total_harvest <- function(df, species_to_remove) {
  total_harvest <- sum(df$Percapita_Pounds_Harvested_sum_avg)
  removed_harvest <- df$Percapita_Pounds_Harvested_sum_avg[df$Lowest_Common_Taxon_Name %in% species_to_remove]
  remaining_harvest <- total_harvest - sum(removed_harvest)
  return(remaining_harvest)
}

# Iterate over each community
communities <- unique(total_harvest_rank$Site)

for (community in communities) {
  cat("Community:", community, "\n")
  
  # Extract harvest data for the current community
  community_data <- subset(total_harvest_rank, Site == community)
  
  # Sort the harvest data within the community from greatest to lowest harvest
  sorted_species <- unique(community_data$Lowest_Common_Taxon_Name[order(-community_data$Percapita_Pounds_Harvested_sum_avg)])
  
  # Initialize variables to store results for the current community
  species_removed <- 0
  remaining_harvest <- sum(community_data$Percapita_Pounds_Harvested_sum_avg)
  
  # Sequential removals of species and calculation of total harvest
  for (species in sorted_species) {
    remaining_harvest <- calculate_total_harvest(community_data, species)
    species_removed <- species_removed + 1
    # Store results for the current community
    results <- rbind(results, data.frame(community = community, species_removed = species_removed, total_harvest = remaining_harvest))
    community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
  }
}

##add row of total harvest w/ no species lost
total_harvest_percap <- df_comm_avg %>%
  ungroup() %>%
  select(Site, Percapita_Pounds_Harvested_sum_avg_total) %>%
  unique() %>%
  rename(total_harvest = "Percapita_Pounds_Harvested_sum_avg_total", community = "Site") %>%
  mutate(species_removed = 0)


results_2 <- rbind(results, total_harvest_percap)

##save outputs
write.csv(results_2, "data/intermediate_data/species_removal_results_raw_percap.csv")

##extract slope and relate to species richness --------NON-LINEAR --------------
##using a self-starting function, which is a special function for curve fitting that guesses its own start parameters 
##(using nls w/ exponential decay can lead to error of singular gradient if bad starting values are picked (and in our case will have unique starting values for each community))
##General exponential decay equation: y(t) ~ yf + (y0 - yf)e ^-at
##Exponential decay parameters: y0 = starting value, yf = final value, a = rate of change, t = time (or in our case # of species removed)

##Now doing for all communities
nls_all <- results_2 %>%
  nest(-community) %>%
  mutate(
    fit = map(data, ~nls(total_harvest ~ SSasymp(species_removed, yf, y0, log_alpha), data = .)),
    tidied = map(fit, tidy),
    augmented = map(fit, augment)
  )

##generate table of fit parameters: y0, yf, alpha
nls_all_param <- nls_all %>%
  unnest(tidied) %>%
  select(community, term, estimate) %>%
  spread(term, estimate) %>%
  mutate(alpha = exp(log_alpha))

##Plot curves for each community
augmented <- nls_all %>%
  unnest(augmented)

qplot(species_removed, total_harvest, data = augmented, geom = 'point', colour = community) +
  geom_line(aes(y=.fitted)) +
  theme(legend.position = "none") +
  facet_wrap(~community)

##Join non-linear parameters to harvest diversity metrics --------------
harvest_div <- read.csv("data/intermediate_data/harvest_diversity_metrics_percap.csv")
rm(list = ls()[!ls() %in% c("nls_all_param", "harvest_div")])

nls_all_param <- nls_all_param %>%
  rename(Site = "community")

comm_div_2 <- left_join(harvest_div, nls_all_param, by = "Site") 

write.csv(comm_div_2, "data/intermediate_data/average_harvest_removal_results_percap.csv")


