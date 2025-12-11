##Compare harvest distributions stability and synchrony to diversity metrics -- PERCAPITA 

library(tidyverse)

##Read in seasonal harvest distributions
simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv")


##Calculating total harvest per date -----------------
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))

total_harvest_all <- total_simulated_harvest %>%
  filter(!is.na(harvest_total)) %>%
  group_by(date) %>%
  summarise_at(vars(harvest_total), list(harvest_mean = mean))


##Calculate Synchrony between harvest taxa across season ---------------
library(codyn)
##add day of year 1-365 for each date 
dates <- simulated_harvest_df %>%
  select(date) %>%
  distinct() %>%
  mutate(date_num = 1:365)

simulated_harvest_df <- left_join(simulated_harvest_df, dates, by = "date")
simulated_harvest_df$date_num <- as.numeric(simulated_harvest_df$date_num)

##Synchrony using Loreau metric
synchrony_loreau <- synchrony(df = simulated_harvest_df, time.var = "date_num", species.var = "species", abundance.var = "harvest_amount", replicate.var = "site", metric = "Loreau") %>%
  dplyr::rename(synchrony_loreau = "synchrony")

synchrony_gross <- synchrony(df = simulated_harvest_df, time.var = "date_num", species.var = "species", abundance.var = "harvest_amount", replicate.var = "site", metric = "Gross") %>%
  dplyr::rename(synchrony_gross = "synchrony")


synchrony_df <- left_join(synchrony_loreau, synchrony_gross, by = "site")

ggplot(synchrony_df, aes(x = synchrony_loreau, y = synchrony_gross)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sync_lm_l_g <- lm(synchrony_gross ~ synchrony_loreau, synchrony_df)
summary(sync_lm_l_g)


##Synchrony between habitats across season -----------
total_simulated_harvest_habitat <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, habitat, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))

##add day of year 1-365 for each date 
dates <- total_simulated_harvest_habitat %>%
  ungroup() %>%
  select(date) %>%
  distinct() %>%
  mutate(date_num = 1:365)

total_simulated_harvest_habitat <- left_join(total_simulated_harvest_habitat, dates, by = "date")
total_simulated_harvest_habitat$date_num <- as.numeric(total_simulated_harvest_habitat$date_num)

##Synchrony using Loreau metric
synchrony_loreau_habitat <- synchrony(df = total_simulated_harvest_habitat, time.var = "date_num", species.var = "habitat", abundance.var = "harvest_total", replicate.var = "site", metric = "Loreau") %>%
  dplyr::rename(synchrony_loreau_habitat = "synchrony")

synchrony_gross_habitat <- synchrony(df = total_simulated_harvest_habitat, time.var = "date_num", species.var = "habitat", abundance.var = "harvest_total", replicate.var = "site", metric = "Gross") %>%
  dplyr::rename(synchrony_gross_habitat = "synchrony")


synchrony_df_habitat <- left_join(synchrony_loreau_habitat, synchrony_gross_habitat, by = "site")


##Calculate variability metrics
##calculating sd and CV
sim_harv_cv <- total_simulated_harvest %>%
  group_by(site) %>%
  summarise(harvest_total_mean = mean(harvest_total), harvest_total_sd = sd(harvest_total), harvest_total_cv = harvest_total_sd/harvest_total_mean)

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_cv), y = harvest_total_cv)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_sd), y = harvest_total_sd)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_mean), y = harvest_total_mean)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


##add seasonal CV/SD to harvest structure metrics df --------------
comm_div <- read.csv("data/intermediate_data/harvest_diversity_metrics_percap.csv") %>%
  rename(site = "Site")
#rm(list = ls()[!ls() %in% c("comm_div_2", "h_df_all")])

comm_dv_cv <- left_join(comm_div, sim_harv_cv, by = "site") %>%
  left_join(harvest_threshold, by = "site")%>%
  left_join(synchrony_df, by = "site") %>%
  left_join(synchrony_df_habitat, by = "site")


write.csv(comm_dv_cv, "data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv")

