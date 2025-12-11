###Harvest structure -- distribution of percapita harvest amount, average of all surveys -- MS Figure


library(tidyverse)
library(ggplot2)

source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")


df_comm_avg <- df_comm_avg %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0)

site_list <- df_comm_avg %>%
  select(Site) %>%
  distinct()
site_list$community_unique <- paste("Community", seq_along(site_list$Site))

df_comm_avg <- left_join(df_comm_avg, site_list, by = "Site")



##For each community/year, create rank # based on percapita of total harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  group_by(Site, community_unique) %>%
  mutate(harvest_rank = rank(-Percapita_Pounds_Harvested_sum_avg, ties.method = "min")) 

total_harvest_rank <- total_harvest_rank %>%
  group_by(Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())

total_harvest_rank_prop <-df_comm_avg %>%
  group_by(Site, community_unique) %>%
  mutate(harvest_rank = rank(-Percapita_Harvest_prop, ties.method = "min")) %>%
  select(Site, Habitat, Lowest_Common_Taxon_Name, Percapita_Harvest_prop, harvest_rank)

###MAIN MS FIGURE  ------------
##colours for habitats:
clrs = c("#EFC3C8", "#8DA0B5","#D7BAA1", "#94B295")
#clrs2 = c("#DF929D", "#B6C9EB", "#C39262", "#AECB98" )
clrs2 = c("#FF9999","#003366","#CC9966", "#339933")
##If want to plot distributions for main MS -- or just put in supllement, not sure 
##Example community -- low diversity/coupling
ex2 <- total_harvest_rank %>%
  filter(Site %in% c("Klukwan")) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = 1:38)


ex2_2 <- total_harvest_rank_prop %>%
  filter(Site %in% c("Klukwan")) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = 1:38)

##Calculate cumulative %
ex2_2 <- ex2_2[order(-ex2_2$Percapita_Harvest_prop),]
ex2_2$percapita_harvest_prop_total <- cumsum(ex2_2$Percapita_Harvest_prop)
##Harvest distribution fig
ex2_plot <- ex2 %>%
  ggplot(aes(x = harvest_rank_2, y= Percapita_Pounds_Harvested_sum_avg, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = clrs2)+
  theme_classic() +
#  facet_wrap(~Site) +
  ylab("Per capita Harvest (kg/person)") +
  xlab("Species") +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 20),axis.title.x=element_text(size = 20),  text = element_text(family = "Avenir"), legend.position = "none")

ex2_plot


##Habitat coupling pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    #   plot.title=element_text(size=14, face="bold")
  )

ex2_2_hab <- ex2_2 %>%
  group_by(Habitat) %>%
  summarise_at(vars(Percapita_Harvest_prop), list(sum))

ex2_piechart <- ggplot(ex2_2_hab, aes(x = "", y = Percapita_Harvest_prop, fill = Habitat)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = clrs2)+
  blank_theme +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.text.y = element_blank())

ex2_piechart
##Example -- high diversity/coupling
ex1 <- total_harvest_rank %>%
  filter(Site %in% c("Klawock")) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = 1:55)

ex1_2 <- total_harvest_rank_prop %>%
  filter(Site %in% c("Klawock")) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = 1:55)

##Calculate cumulative %
ex1_2 <- ex1_2[order(-ex1_2$Percapita_Harvest_prop),]
ex1_2$percapita_harvest_prop_total <- cumsum(ex1_2$Percapita_Harvest_prop)

##Harvest distribution fig
ex1_plot <- ex1%>%
  ggplot(aes(x = harvest_rank_2, y= Percapita_Pounds_Harvested_sum_avg, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values =clrs2)+
  theme_classic() +
#  facet_wrap(~Site) +

  ylab("Per capita Harvest (kg/person)") +
  xlab("Species") +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 20),axis.title.x=element_text(size = 20),  text = element_text(family = "Avenir"), legend.position = "none")
ex1_plot


##Habitat coupling pie chart
ex1_2_hab <- ex1_2 %>%
  group_by(Habitat) %>%
  summarise_at(vars(Percapita_Harvest_prop), list(sum))

ex1_piechart <- ggplot(ex1_2_hab, aes(x = "", y = Percapita_Harvest_prop, fill = Habitat)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = clrs2)+
  blank_theme +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.text.y = element_blank())
 
ex1_piechart


##Combine the harvest distribution and pie charts for each example
library(ggpubr)
ex1_fig_3 <- ggarrange(ex1_plot, ex1_piechart, nrow = 1, ncol = 2, labels = c("i)", "ii)"), font.label = list(colour = "black", size = 22, family = "Avenir"))
ex1_fig_3
ex2_fig_3 <- ggarrange(ex2_plot, ex2_piechart, nrow = 1, ncol = 2, labels = c("i)", "ii)"), font.label = list(colour = "black", size = 22, family = "Avenir"))
ex2_fig_3

ggsave("figures/figure_3B.png", plot = ex1_fig_3)
ggsave("figures/figure_3A.png", plot = ex2_fig_3)

###SUPPLEMENTAL FIGURE -----------
##Plot harvest distributions by community
total_harvest_rank$community_unique <- factor(total_harvest_rank$community_unique, levels = paste("Community", 1:46))

total_harvest_rank <- total_harvest_rank %>%
  group_by(community_unique, Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())


fig_s2<- ggplot(total_harvest_rank, aes(x = harvest_rank_2, y= Percapita_Pounds_Harvested_sum_avg, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = clrs2)+
  theme_classic() +
  xlab("Harvest Rank") +
  ylab("Per capita Harvest (kg/person)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Avenir")) +
  facet_wrap(~community_unique, scale = "free")
fig_s2

ggsave("figures/figure_S2.png", plot = ex2_fig_3)

