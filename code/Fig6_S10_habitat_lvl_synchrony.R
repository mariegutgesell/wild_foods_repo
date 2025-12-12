###Synchrony Figure 

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(codyn)
###2) AVERAGE HARVEST OVER TIME -- SYNCHRONY  
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(Forest:synchrony_gross_habitat)
df_2 <- read.csv("data/intermediate_data/average_harvest_removal_results_percap.csv") %>%
  rename(site = "Site") %>%
  select(Forest:alpha)

avg_df <- left_join(df_1, df_2, by = c("Forest", "site", "richness", "sw_diversity", "evenness", "avg", "sd", "sw_div_h", "richness_h", "evenness_h", "evenness_h_log", "sd_log", "log_1_SD")) 
rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])


##SYNCHRONY (GROSS) vs. CV
avg_sync_cv <- ggplot(avg_df, aes(x = synchrony_gross_habitat, y = harvest_total_cv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Seasonal Habitat Synchrony (Gross)") +
  ylab("Per capita Harvest CV\n(seasonal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
avg_sync_cv


###Linear Regressions 
avg_sync_g_cv_lm <- lm(harvest_total_cv ~ synchrony_gross_habitat, avg_df)
summary(avg_sync_g_cv_lm)

avg_sync_l_cv_lm <- lm(harvest_total_cv ~ synchrony_loreau_habitat, avg_df)
summary(avg_sync_l_cv_lm)



#####DECADAL SYNCHRONY ACROSS HABITAT ---------------
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
  rename(site = "Site")

###Calculate synchrony between habitats over time -- right this makes sense to do for aggregated ... not for each individual species.. 
str(df_h)
df_h$Year <- as.numeric(df_h$Year)
decadal_synchrony_loreau <- synchrony(df = df_h, time.var = "Year", species.var = "Habitat", abundance.var = "percapita_kg", replicate.var = "site", metric = "Loreau") %>%
  dplyr::rename(synchrony_loreau = "synchrony")

decadal_synchrony_gross <- synchrony(df = df_h, time.var = "Year", species.var = "Habitat", abundance.var = "percapita_kg", replicate.var = "site", metric = "Gross") %>%
  dplyr::rename(synchrony_gross = "synchrony")


decadal_synchrony_df <- left_join(decadal_synchrony_loreau, decadal_synchrony_gross, by = "site") %>%
  rename(Community = "site")

##Calculate mean, sd and CV of total harvest across time 
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

##Calculate mean harvest structure metrics, and join to CV of total harvest
mean_hc <- df_hc %>%
  select(Site, richness, sw_diversity, evenness, sd, log_1_SD, sd_0_1) %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd, log_1_SD, sd_0_1), list(mean = mean)) %>%
  rename(Community ="Site")


mean_hc_cv <- left_join(mean_hc, harvest_cv, by = "Community") %>%
  left_join(decadal_synchrony_df)

####Plotting
##SYNCHRONY (GROSS) vs. SW diversity and coupling
dec_sync_cv <- ggplot(mean_hc_cv, aes(x = synchrony_gross, y = cv_ph)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Interannual Habitat Synchrony (Gross)") +
  ylab("Per capita Harvest CV\n(interannual)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
dec_sync_cv

###Linear Regressions 
dec_sync_g_cv_lm <- lm(cv_ph ~ synchrony_gross, mean_hc_cv)
summary(dec_sync_g_cv_lm)

dec_sync_l_cv_lm <- lm(cv_ph ~ synchrony_loreau, mean_hc_cv)
summary(dec_sync_l_cv_lm)

###GROSS SYNCHRONY FIG -- DECADAL AND AVERAGE SYNCHRONY VS CV and Portfolio effect boxplots
source("code/habitat_lvl_portfolio_effect_boxplot.R")
#rm(list = ls()[!ls() %in% c("dec_sync_cv", "avg_sync_cv", "harvest_cv_all")])

seasonal_pf <- harvest_cv_all %>%
  filter(time_type == "Seasonal") %>%
  ggplot( aes(x = `CV Type`, y = CV, fill = `CV Type`)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "white", "darkgray"))+
  theme_classic() +
  ylab("Per capita Harvest CV\n(seasonal)") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_blank(), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
seasonal_pf

decadal_pf <- harvest_cv_all %>%
  filter(time_type == "Decadal") %>%
  ggplot( aes(x = `CV Type`, y = CV, fill = `CV Type`)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "white", "darkgray"))+
  theme_classic() +
  ylab("Per capita Harvest CV\n(interannual)") + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_blank(), text = element_text(family = "Avenir"), strip.background = element_blank(), legend.position = "none")
decadal_pf


##Bring in time series of mean harvest across habitats 
source("code/mean_seasonal_habitat_phenology_plot.R")
seasonal_plot_mean_3
ia_plot_mean_3

avg_decadal_synchrony_fig_2 <-  ggarrange(seasonal_plot_mean_3, avg_sync_cv, seasonal_pf, ia_plot_mean_3, dec_sync_cv, decadal_pf,
                                        nrow = 2, ncol = 3,  font.label = list(colour = "black", size = 14, family = "Avenir"))
avg_decadal_synchrony_fig_2

ggsave("figures/figure_6.png", avg_decadal_synchrony_fig_2)





###Statistical Tests ----- 

##seasonal portfolio effect anovas 
library(car)
seasonal_pf_df  <- harvest_cv_all %>%
  filter(time_type == "Seasonal") %>%
  mutate(CV_log = log(CV))
hist(seasonal_pf_df$CV)
hist(seasonal_pf_df$CV_log)

leveneTest(CV ~ cv_type, data = seasonal_pf_df) ##check for homogeneity of variance
shapiro.test(seasonal_pf_df$CV) ##check for normality of residuals 
by(seasonal_pf_df$CV, seasonal_pf_df$cv_type, shapiro.test)

##sample size in each group is large enough that anova should be robust to lack of normality, and groups do not violate homogeneity assumption so i think okay to go ahead
seasonal_pf_aov <- aov(CV ~ cv_type, seasonal_pf_df)
summary(seasonal_pf_aov)

seasonal_pf_kw <- kruskal.test(CV ~ cv_type, seasonal_pf_df)
seasonal_pf_kw

summary(seasonal_pf_kw)



TukeyHSD(seasonal_pf_aov)
check_homogeneity(seasonal_pf_aov)
check_normality(seasonal_pf_aov)

##log transformation does not increase normality/difference in variances b/w groups 


##portfolio effect ANOVAs 
decadal_pf_df  <- harvest_cv_all %>%
  filter(time_type == "Decadal") 

leveneTest(CV ~ cv_type, data = decadal_pf_df) ##check for homogeneity of variance
shapiro.test(decadal_pf_df$CV) ##check for normality of residuals 
by(decadal_pf_df$CV, decadal_pf_df$cv_type, shapiro.test)


decadal_pf_aov <- aov(CV ~ cv_type, decadal_pf_df)
summary(decadal_pf_aov)

TukeyHSD(decadal_pf_aov)

check_homogeneity(decadal_pf_aov)
check_normality(decadal_pf_aov)

decadal_pf_kw <- kruskal.test(CV ~ cv_type, decadal_pf_df)
decadal_pf_kw


##USING KW test as passed homogeneity of variance but failed normality test 


##########LOREAU -------------
####LOREAU -- AVERAGE
avg_sync_cv <- ggplot(avg_df, aes(x = synchrony_loreau_habitat, y = harvest_total_cv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Seasonal Habitat Synchrony (Loreau)") +
  ylab("Per capita Harvest CV\n(seasonal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
avg_sync_cv


####LOREAU -DECADAL
dec_sync_cv <- ggplot(mean_hc_cv, aes(x = synchrony_loreau, y = cv_ph)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Interannual Habitat Synchrony (Loreau)") +
  ylab("Per capita Harvest CV\n(interannual)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Avenir"), strip.background = element_blank())
dec_sync_cv


loreau_syncrhony_fig <- ggarrange(avg_sync_cv, dec_sync_cv, nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Avenir"))
loreau_syncrhony_fig

ggsave("figures/figure_S10.png", loreau_syncrhony_fig)


