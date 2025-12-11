##Harvest PHenology/Time Series Figure

library(ggplot2)
library(tidyverse)
library(ggpubr)


###SEASONAL HARVEST PHENOLOGY ---------------

##Read in whichever harvest distributions (i.e., based on proportions, total lbs harvested or percapita)
##Proportion of total harvest
simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv") %>%
  mutate(harvest_amount_kg = harvest_amount*0.45359237)

site_list <- simulated_harvest_df %>%
  select(site) %>%
  distinct()
site_list$community_unique <- paste("Community", seq_along(site_list$site))

simulated_harvest_df <- left_join(simulated_harvest_df, site_list, by = "site")

##Calculating total harvest per date -----------------
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, community_unique, date) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_total = sum))


##Representative Communities: Klukwan and Metlaktla 
##Klukwan 
seasonal_example1 <- simulated_harvest_df %>%
  filter(site == "Klukwan")

seasonal_example1_total <- total_simulated_harvest %>%
  filter(site == "Klukwan")

##Klukwan Phenology Plot

seasonal_ex1_plot <- ggplot() +
  geom_line(data = seasonal_example1, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = seasonal_example1_total, aes(x = date, y = harvest_total), linewidth = 1, colour = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Calendar Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  ylim(0,3.0) +
  theme(panel.background = element_rect(fill = NA, color = NA),   # transparent panel
        plot.background = element_rect(fill = NA, color = NA), #transparent full plot
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir")) +
  theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
seasonal_ex1_plot


seasonal_example2 <- simulated_harvest_df %>%  filter(site == "Angoon")
seasonal_example2_total <- total_simulated_harvest %>%
  filter(site == "Angoon")

seasonal_ex2_plot <- ggplot() +
  geom_line(data = seasonal_example2, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = seasonal_example2_total, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Calendar Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  ylim(0, 0.8) +
  theme( panel.background = element_rect(fill = NA, color = NA),   # transparent panel
         plot.background = element_rect(fill = NA, color = NA), #transparent full plot
         axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir"))+
  theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
seasonal_ex2_plot


##DECADAL HARVEST TIME SERIES -----------------
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

df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  mutate(percapita_kg = Percapita_Pounds_Harvested_sum_total*0.45359237) %>%
  rename(site = "Site") %>%
  left_join(site_list, by = "site")


#####EXAMPLE TIME SERIES FOR MS FIGURE
##angoon
df_example_1 <- df_h %>%
  filter(site %in% c("Klukwan"))
df_sum_example_1 <- df_example_1 %>%
  group_by(site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))

df_example_1$Year <- as.numeric(df_example_1$Year)
df_sum_example_1$Year <- as.numeric(df_sum_example_1$Year)
decadal_example1_plot <- ggplot() +
  geom_line(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat), linewidth = 1) +
  geom_point(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat), size =2) +
 # geom_bar(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, fill = Habitat), stat = "identity") +
  scale_color_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_sum_example_1, aes(x = Year, y = total_percapita, group = site), linewidth=1) +
  geom_point(data = df_sum_example_1, aes(x = Year, y = total_percapita, group = site), size = 2) +
  theme_classic() + 
  ylab("Annual Harvest Amount\n(kg/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
#  ylim(0,610) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_blank(),  text = element_text(family = "Avenir"), legend.position  = "none") +
  theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
decadal_example1_plot

##Angoon
df_example_2 <- df_h %>%
  filter(site %in% c("Angoon"))
df_sum_example_2 <- df_example_2 %>%
  group_by(site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))

df_example_2$Year <- as.numeric(df_example_2$Year)
df_sum_example_2$Year <- as.numeric(df_sum_example_2$Year)
decadal_example2_plot <- ggplot() +
  geom_line(data = df_example_2, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat), linewidth = 1) +
  geom_point(data = df_example_2, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat), size = 2) +
  #geom_bar(data = df_example_2, aes(x = Year, y = percapita_kg, group = Habitat, fill = Habitat), stat = "identity") +
  scale_color_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_sum_example_2, aes(x = Year, y = total_percapita, group = site), linewidth = 1) +
  geom_point(data = df_sum_example_2, aes(x = Year, y = total_percapita, group = site), size = 2) +
  theme_classic() + 
  ylab("Annual Harvest Amount\n(kg/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_blank(),  text = element_text(family = "Avenir"), legend.position  = "none")+
  theme(plot.margin = unit(c(0.25, 1, 0.25, 0.25), "cm"))
decadal_example2_plot

df_sum_all <- df_h %>%
  group_by(site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))

df_sum_all$Year <- as.numeric(df_sum_all$Year)
decadal_all_plot <- ggplot() +
  geom_line(data = df_sum_all, aes(x = Year, y = total_percapita, group = site)) +
  geom_point(data = df_sum_all, aes(x = Year, y = total_percapita, group = site)) +
  theme_classic() + 
  ylab("Annual Harvest Amount\n(kg/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Avenir"), legend.position  = "none") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
decadal_all_plot

##Final plot 
##Create figure legend for 4 habitats (block) and black total harvest line 
data <- data.frame(
  Xdata = rnorm(4),
  Ydata = rnorm(4),
  LegendData = c("Freshwater/Anadromous", "Marine", "Intertidal", "Terrestrial")
)
data$LegendData <- factor(data$LegendData, levels = c("Freshwater/Anadromous", "Marine", "Intertidal", "Terrestrial"))
gplot <- ggplot(data, aes(Xdata, Ydata, fill = LegendData)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933")) +
  theme_classic()+
  guides(fill = guide_legend(title = "Habitat")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, family = "Avenir"), legend.position = "bottom")
gplot

leg_fig <- get_legend(gplot)


temporal_fig <- ggarrange(seasonal_ex1_plot, seasonal_ex2_plot, decadal_example1_plot, decadal_example2_plot, legend = "bottom", common.legend = TRUE, legend.grob = leg_fig,
                   labels = c("i)", "i)", "ii)", "ii)"),
                   ncol = 2, nrow = 2, font.label = list(colour = "black", size = 14, family = "Avenir"))
temporal_fig

ggsave("figures/figure_4.png", plot = temporal_fig)

###SUPPLEMENTAL FIGURES ---------
##Seasonal harvest phenology -- all communities 
simulated_harvest_df$community_unique <- factor(simulated_harvest_df$community_unique, levels = paste("Community", 1:46))
total_simulated_harvest$community_unique <- factor(total_simulated_harvest$community_unique, levels = paste("Community", 1:46))

fig_s3 <- ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  #  ylim(0, 6.4) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir")) +
  facet_wrap(~community_unique, scale = "free")
fig_s3

ggsave("figures/figure_S3.png", plot = fig_s3)
##Decadal Harvest -- all communities

df_h_summary <- df_h %>%
  group_by(site, community_unique, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum)) 

df_h$community_unique <- factor(df_h$community_unique, levels = paste("Community", 1:46))
df_h_summary$community_unique <- factor(df_h_summary$community_unique, levels = paste("Community", 1:46))

df_h$Year <- as.numeric(df_h$Year)
df_h_summary$Year <- as.numeric(df_h_summary$Year)
fig_s4 <- ggplot() +
  geom_line(data = df_h, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  geom_point(data = df_h, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_h_summary, aes(x = Year, y = total_percapita, group = site)) +
  geom_point(data = df_h_summary, aes(x = Year, y = total_percapita, group = community_unique)) +
  theme_classic() + 
  ylab("Annual Harvest Amount\n(kg/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Avenir"), legend.position  = "none") +
  facet_wrap(~community_unique)
fig_s4

ggsave("figures/figure_S4.png", plot = fig_s4)
