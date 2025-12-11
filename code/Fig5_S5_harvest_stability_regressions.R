##Harvest Stability Figure 

library(tidyverse)
library(ggplot2)
library(ggpubr)

##1) DECADAL HARVEST CV AND MEAN HARVEST DIVERSITY AND COUPLING -------------------
##Import temporal harvest structure metrics

df_hc <- read.csv("data/intermediate_data/temporal_harvest_diversity_metrics_percap.csv") %>%
  separate(Site_Year_Code, into = c("Site", "Year"), sep = "_", remove = FALSE)

##select only sites w/ more than three years of data
df_hc <- df_hc %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site != "Valdez") ##removing because 3 surveys only span 3 years (want to look across decade)

##Calculate cv of interannual harvest 
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
  select(Community, mean_ph:cv_ph) %>%
  distinct()

##Mean harvest 
mean_hc <- df_hc %>%
  select(Site, richness, sw_diversity, evenness, sd, log_1_SD, sd_0_1, logit_sd_0_1) %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd, log_1_SD, sd_0_1, logit_sd_0_1), list(mean = mean)) %>%
  rename(Community ="Site") %>%
  mutate(example_site = ifelse(Community %in% c("Angoon", "Klukwan" ), "yes", "no")) %>%
  mutate(example_site_color = case_when(
    startsWith(Community, "Ang") ~ "Green",
    startsWith(Community, "Kluk") ~ "Red",
    startsWith(example_site, "no") ~ "Black",
  ))

mean_hc_cv <- left_join(mean_hc, harvest_cv, by = "Community")

#rm(list = ls()[!ls() %in% c("mean_hc_cv")])


###2) AVERAGE HARVEST OVER TIME -- 
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(Forest:synchrony_gross)
df_2 <- read.csv("data/intermediate_data/average_harvest_removal_results_percap.csv") %>%
  rename(site = "Site") %>%
  select(Forest, site, log_alpha:alpha)



avg_df <- left_join(df_1, df_2, by = c("Forest", "site")) %>%
  mutate(example_site = ifelse(site %in% c("Metlakatla", "Klukwan"), "yes", "no")) %>%
  mutate(example_site_color = case_when(
    startsWith(site, "Met") ~ "Green",
    startsWith(site, "Kluk") ~ "Red",
    startsWith(example_site, "no") ~ "Black",
  ))
#rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])

##save table for manuscript of metrics 
avg_df$community_unique <- paste("Community", seq_along(avg_df$site))
ms_table <- avg_df %>%
  rename(Community = "site") %>%
  left_join(mean_hc_cv, by = "Community") %>%
  select(community_unique, sw_diversity, richness, evenness, sd_0_1, synchrony_gross, alpha, harvest_total_cv, sw_diversity_mean, richness_mean, evenness_mean, sd_0_1_mean, cv_ph)

#write.csv(ms_table, "tables/harvest_metrics_stability_results.csv")

##3) PLOTTING FIGURE 5 -------------
##3.1) Robustness vs. SW diversity and coupling
avg_robust_coupling <- ggplot(avg_df, aes(x = sd_0_1, y = alpha)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
 # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  #xlim(0, 1)+
  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
avg_robust_coupling

avg_robust_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = alpha)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
#  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  xlim(1.92, 3.03) + 
  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
avg_robust_diversity


avg_robust_synchrony <- ggplot(avg_df, aes(x = synchrony_gross, y = alpha)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Species Synchrony (Gross)") +
  ylab("Rate of Harvest\nDecline (alpha)") +
 # xlim(1.92, 3.03) + 
#  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
avg_robust_synchrony


##3.2) Seasonal CV vs. SW diversity and coupling
avg_cv_coupling <- ggplot(avg_df, aes(x = sd_0_1, y = harvest_total_cv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  #scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling") +
  ylab("Per capita Harvest CV \n (seasonal)") +
 # ylim(0.5, 1.55) +
 # xlim(-3.7,-1.43)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(panel.background = element_rect(fill = NA, color = NA),   # transparent panel
        plot.background = element_rect(fill = NA, color = NA), 
        axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
avg_cv_coupling

avg_cv_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = harvest_total_cv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
 # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Per capita Harvest CV \n (seasonal)") +
  xlim(1.92, 3.03) + 
  ylim(0.5,1.55)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(panel.background = element_rect(fill = NA, color = NA),   # transparent panel
        plot.background = element_rect(fill = NA, color = NA), 
        axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
avg_cv_diversity


##3.3) Decadal CV vs. SW diversity and coupling
decadal_cv_coupling <- ggplot(mean_hc_cv, aes(x = sd_0_1_mean, y = cv_ph )) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
 # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Mean Habitat Coupling") +
  ylab("Per capita Harvest CV \n (interannual)") +
  ylim(0, 0.63) +
 # xlim(-3.7,-1.43)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
decadal_cv_coupling

decadal_cv_diversity <- ggplot(mean_hc_cv, aes(x = sw_diversity_mean, y = cv_ph)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
#  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("Per capita Harvest CV \n (interannual)") +
  xlim(1.92, 3.03) + 
  ylim (0, 0.63) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
decadal_cv_diversity


##Combine into 1 figure
fig_5 <-  ggarrange(avg_cv_diversity, avg_cv_coupling, decadal_cv_diversity, decadal_cv_coupling,avg_robust_diversity, avg_robust_coupling, nrow = 3, ncol = 2, labels = c("i)", "ii)", "i)", "ii)", "i)", "ii)"), font.label = list(colour = "black", size = 14, family = "Avenir"))
fig_5

ggsave("figures/figure_5.png", plot = fig_5)
###4) REGRESSION ANALYSIS ------------
avg_robust_div_lm <- lm(alpha ~ sw_diversity, avg_df)

summary(avg_robust_div_lm)

avg_robust_coupling_lm <- lm(alpha~ logit_sd_0_1, avg_df)
summary(avg_robust_coupling_lm)

avg_cv_div_lm <- lm(harvest_total_cv ~ sw_diversity, avg_df)
summary(avg_cv_div_lm)

avg_cv_coupling_lm <- lm(harvest_total_cv ~ logit_sd_0_1, avg_df)
summary(avg_cv_coupling_lm)

dec_cv_div_lm <- lm(cv_ph ~ sw_diversity_mean, mean_hc_cv)
summary(dec_cv_div_lm)

dec_cv_coupling_lm <- lm(cv_ph ~ logit_sd_0_1_mean, mean_hc_cv)
summary(dec_cv_coupling_lm)



##5) PLOTTING SUPPLEMENTAL FIGURE -- Partitioning SW diversity into richness and evenness  ---------
##robustness
avg_robust_richness <- ggplot(avg_df, aes(x = richness, y = alpha)) +
  geom_point(size =2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Rate of Harvest\nDecline (alpha)") +
 # xlim(1.9, 3.0) + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())

avg_robust_richness

avg_robust_evenness <- ggplot(avg_df, aes(x = evenness, y = alpha)) +
  geom_point(size =2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  # xlim(1.9, 3.0) + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
avg_robust_evenness

##seasonal CV
avg_cv_richness <- ggplot(avg_df, aes(x = richness, y = harvest_total_cv)) +
  geom_point(size = 2) +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Per capita Harvest CV\n(seasonal)") +
  # xlim(1.9, 3.0) + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
avg_cv_richness

avg_cv_evenness <- ggplot(avg_df, aes(x = evenness, y = harvest_total_cv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Per capita Harvest CV\n(seasonal)") +
  # xlim(1.9, 3.0) + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
avg_cv_evenness

##decadal CV
decadal_cv_richness <- ggplot(mean_hc_cv, aes(x = richness_mean, y = cv_ph)) +
  geom_point(size = 2) +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Richness") +
  ylab("Per capita Harvest CV\n(interannual)") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
decadal_cv_richness

decadal_cv_evenness <- ggplot(mean_hc_cv, aes(x = evenness_mean, y = cv_ph)) +
  geom_point(size = 2) +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Evenness") +
  ylab("Per capita Harvest CV\n(interannual)") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
decadal_cv_evenness

supp_fig_S5 <-  ggarrange(avg_robust_richness, avg_robust_evenness, avg_cv_richness, avg_cv_evenness, decadal_cv_richness, decadal_cv_evenness, nrow = 3, ncol = 2, labels = c("i)", "ii)", "i)", "ii)", "i)", "ii)"), font.label = list(colour = "black", size = 14, family = "Avenir"))
supp_fig_S5

ggsave("figures/figure_S5.png", plot = supp_fig_S5)
##6) REGRESSION ANALYSIS --- richness and evenness --------
avg_robust_rich_lm <- lm(alpha ~ richness, avg_df)
summary(avg_robust_rich_lm)

avg_robust_even_lm <- lm(alpha~ evenness, avg_df)
summary(avg_robust_even_lm)

avg_cv_rich_lm <- lm(harvest_total_cv ~ richness, avg_df)
summary(avg_cv_rich_lm)

avg_cv_even_lm <- lm(harvest_total_cv ~ evenness, avg_df)
summary(avg_cv_even_lm)

dec_cv_rich_lm <- lm(cv_ph ~ richness_mean, mean_hc_cv)
summary(dec_cv_rich_lm)

dec_cv_even_lm <- lm(cv_ph ~ evenness_mean, mean_hc_cv)
summary(dec_cv_even_lm)




