##Generate Harvest Structures for Two example Communities (averaged across years) 
##Figure 3 
library(igraph)
##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])


##Harvest Structure for Klukwan w/ groups as general categories and habitat ----------
kluk <- df_comm_avg %>%
  filter(Site == "Klukwan") %>%
  mutate(Resource = case_when(
    grepl("Trout", Lowest_Common_Taxon_Name) ~ "Trout",
    grepl( "Salmon", Lowest_Common_Taxon_Name) ~ "Salmon",
    startsWith(Lowest_Common_Taxon_Name, "Smelt") ~ "Smelt",
    startsWith(Lowest_Common_Taxon_Name, "Dolly") ~ "Trout",
    startsWith(Lowest_Common_Taxon_Name, "Shrimp") ~ "Shrimp",
    startsWith(Lowest_Common_Taxon_Name, "Dungeness") ~ "Dungeness Crab",
    grepl( "Crab", Lowest_Common_Taxon_Name) ~ "Crab",
    startsWith(Lowest_Common_Taxon_Name, "Flounder") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Grayling") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Herring Roe") ~ "Herring Roe",
    startsWith(Lowest_Common_Taxon_Name, "Herring") ~ "Herring",
    startsWith(Lowest_Common_Taxon_Name, "Sculpin") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Rockfish") ~ "Rockfish",
    startsWith(Lowest_Common_Taxon_Name, "Halibut") ~ "Halibut",
    startsWith(Lowest_Common_Taxon_Name, "Seal") ~ "Marine Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Seaweed") ~ "Seaweed/Kelp",
    startsWith(Lowest_Common_Taxon_Name, "Chiton") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Clam") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Cockle") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Clam") ~ "Marine Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Berries") ~ "Berries",
    startsWith(Lowest_Common_Taxon_Name, "Plants") ~ "Plants/Greens",
    startsWith(Lowest_Common_Taxon_Name, "Deer") ~ "Deer",
    startsWith(Lowest_Common_Taxon_Name, "Goat") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Moose") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Black") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Hare") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Porc") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Swan") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Duck") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Geese") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Upland") ~ "Inland Birds",
  )) %>%
  group_by(Habitat, Resource) %>%
  filter(!is.na(Trophic_Level)) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum_avg, Trophic_Level), list(total_percap = sum, mean_TL = mean)) %>%
  select(Habitat, Resource, Percapita_Pounds_Harvested_sum_avg_total_percap, Trophic_Level_mean_TL) %>%
  rename(total_percap = "Percapita_Pounds_Harvested_sum_avg_total_percap", mean_TL = "Trophic_Level_mean_TL")


##Create harvest structure -- using lowest common taxa to start
##1) need to generate interaction matrix 
mat <- kluk %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, total_percap)

##To make sure all lines are visiable, make consistent widt hfor less than 2 kg/person
is1$total_percap <- ifelse(is1$total_percap <5, 5, is1$total_percap)

tl <- mat %>%
  ungroup() %>%
  select(Resource, mean_TL, Habitat)

tl$mean_TL <- as.character(tl$mean_TL)

##set x coordinates
tl <- tl %>%
  mutate(Resource_cat = c(13,15,17, 30:36, 23:26, 1:6))

##add humans
tl[nrow(tl) + 1,] = list("Human", "5", "NA", 15)

tl <- tl %>%
  mutate(Habitat_cat = case_when(
    startsWith(Habitat, "Fresh") ~ "4",
    startsWith(Habitat, "Mar") ~ "1",
    startsWith(Habitat, "Near") ~ "2",
    startsWith(Habitat, "Terr") ~ "5",
    startsWith(Habitat, "NA") ~ "3",
  ))

tl$Habitat_cat <- as.numeric(tl$Habitat_cat)

tl$Resource_cat <- as.numeric(tl$Resource_cat)
str(tl)

tl$Habitat <- ordered(tl$Habitat,
                      levels = c("Marine", "Nearshore",  "NA", "Freshwater_Anadromous", "Terrestrial"))


##Plot with igraph
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

##Generate Colours based on Habitat 
colrs <- c("#003366","#CC9966","black", "#FF9999","#339933")
V(net)$color <- colrs[V(net)$Habitat_cat] ##has to be numeric to assign colors this way

V(net)$Habitat_cat
V(net)$color

#Set edge width based on weight
E(net)$width <- E(net)$total_percap/5


plot(net, edge.arrow.size = 0.5, edge.curved = 0.5, vertex.label = V(net)$Resource)


##color edges of graph based on source node color
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- colrs[V(net)$Habitat_cat[edge.start]]

##want to create a matrix of coordinates, where y is the trophic level, and x is the habitat
tl$mean_TL <- as.numeric(tl$mean_TL)
lay <- matrix(nrow = nrow(tl), ncol = 2)
lay[,1] <- tl$Resource_cat
lay[,2] <- tl$mean_TL
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 15, layout = lay)
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, layout = lay, vertex.label = NA, edge.arrow.size = 2, vertex.frame.color = "white", asp = 0)


##Harvest Structure for Klawock w/ groups as general categories and habitat ---------------
kla <- df_comm_avg %>%
  filter(Site == "Klawock") %>%
  mutate(Resource = case_when(
    grepl("Trout", Lowest_Common_Taxon_Name) ~ "Trout",
    startsWith(Lowest_Common_Taxon_Name,"Unknown Non-Salmon") ~ "Other Marine Fish",
    grepl( "Salmon", Lowest_Common_Taxon_Name) ~ "Salmon",
    startsWith(Lowest_Common_Taxon_Name, "Smelt") ~ "Smelt",
    startsWith(Lowest_Common_Taxon_Name, "Dolly") ~ "Trout",
    startsWith(Lowest_Common_Taxon_Name, "Shrimp") ~ "Shrimp",
    startsWith(Lowest_Common_Taxon_Name, "Dungeness") ~ "Dungeness Crab",
    grepl( "Crab", Lowest_Common_Taxon_Name) ~ "Crab",
    startsWith(Lowest_Common_Taxon_Name, "Flounder") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Grayling") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Herring Roe") ~ "Herring Roe",
    startsWith(Lowest_Common_Taxon_Name, "Herring") ~ "Herring",
    startsWith(Lowest_Common_Taxon_Name, "Sculpin") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Rock Green") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Gadif") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Sable") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Shark") ~ "Shark",
    startsWith(Lowest_Common_Taxon_Name, "Skate") ~ "Skates",
    startsWith(Lowest_Common_Taxon_Name, "Sole") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Unknown Non-Salmon") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "White Sturgeon") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Lingcod") ~ "Other Marine Fish",
    startsWith(Lowest_Common_Taxon_Name, "Rockfish") ~ "Rockfish",
    startsWith(Lowest_Common_Taxon_Name, "Halibut") ~ "Halibut",
    startsWith(Lowest_Common_Taxon_Name, "Seal") ~ "Marine Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Bowhead") ~ "Marine Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Seaweed") ~ "Seaweed/Kelp",
    startsWith(Lowest_Common_Taxon_Name, "Chiton") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Clam") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Cockle") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Abalone") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Mussel") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Sea Urchi") ~ "Sea Urchin",
    startsWith(Lowest_Common_Taxon_Name, "Sea Cucumber") ~ "Sea Cucumber",
    startsWith(Lowest_Common_Taxon_Name, "Limpet") ~ "Nearshore Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Scallop") ~ "Marine Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Oyster") ~ "Marine Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Shrimp") ~ "Marine Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Unknown Marine Invert") ~ "Marine Invertebrates",
    startsWith(Lowest_Common_Taxon_Name, "Octopus") ~ "Octopus/Squid",
    startsWith(Lowest_Common_Taxon_Name, "Squid") ~ "Octopud/Squid",
    startsWith(Lowest_Common_Taxon_Name, "Berries") ~ "Berries",
    startsWith(Lowest_Common_Taxon_Name, "Plants") ~ "Plants/Greens",
    startsWith(Lowest_Common_Taxon_Name, "Deer") ~ "Deer",
    startsWith(Lowest_Common_Taxon_Name, "Goat") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Moose") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Black") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Hare") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Porc") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Beaver") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Squirrel") ~ "Small Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Swan") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Crane") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Duck") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Geese") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Upland") ~ "Inland Birds",
    grepl( "Egg", Lowest_Common_Taxon_Name) ~ "Bird Eggs",
    startsWith(Lowest_Common_Taxon_Name, "Seabirds") ~ "Seabirds",
    startsWith(Lowest_Common_Taxon_Name, "Unknown Other Birds") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Unknown Shorebirds") ~ "Seabirds",
    startsWith(Lowest_Common_Taxon_Name, "Upland Game Birds") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Wilson") ~ "Inland Birds",
    startsWith(Lowest_Common_Taxon_Name, "Brown") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Lynx") ~ "Other Large Land Mammals",
    startsWith(Lowest_Common_Taxon_Name, "Caribou") ~ "Other Large Land Mammals",
    
  )) %>%
  group_by(Habitat, Resource) %>%
  filter(!is.na(Trophic_Level)) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum_avg, Trophic_Level), list(total_percap = sum, mean_TL = mean)) %>%
  select(Habitat, Resource, Percapita_Pounds_Harvested_sum_avg_total_percap, Trophic_Level_mean_TL) %>%
  rename(total_percap = "Percapita_Pounds_Harvested_sum_avg_total_percap", mean_TL = "Trophic_Level_mean_TL")


##Create harvest structure -- using lowest common taxa to start
##1) need to generate interaction matrix 
mat <- kla %>%
  mutate(Consumer = "Human") 

is1 <- mat %>%
  ungroup() %>%
  select(Resource,  Consumer, total_percap)

##To make sure all lines are visiable, make consistent widt hfor less than 2 kg/person
is1$total_percap <- ifelse(is1$total_percap <5, 5, is1$total_percap)

tl <- mat %>%
  ungroup() %>%
  select(Resource, mean_TL, Habitat)

tl$mean_TL <- as.character(tl$mean_TL)

#set x coordinates
tl <- tl %>%
  mutate(Resource_cat = c(13,15,17, 33:43, 23:28, 1:6))

tl[nrow(tl) + 1,] = list("Human", "5", "NA", 15)

tl <- tl %>%
  mutate(Habitat_cat = case_when(
    startsWith(Habitat, "Fresh") ~ "4",
    startsWith(Habitat, "Mar") ~ "1",
    startsWith(Habitat, "Near") ~ "2",
    startsWith(Habitat, "Terr") ~ "5",
    startsWith(Habitat, "NA") ~ "3",
  ))

tl$Habitat_cat <- as.numeric(tl$Habitat_cat)

tl$Resource_cat <- as.numeric(tl$Resource_cat)

tl$Habitat <- ordered(tl$Habitat,
                      levels = c("Marine", "Nearshore",  "NA", "Freshwater_Anadromous", "Terrestrial"))


##Plot with igraph
net <- graph_from_data_frame(d= is1, vertices = tl, directed = T)
class(net)
net

E(net)
V(net)

plot(net, edge.arrow.size = .4, vertex.label = NA)

##replace the vertex names
plot(net, edge.arrow.size = 0.2, edge.curved = 0, vertex.label = V(net)$Resource)

##Generate Colours based on Habitat 
colrs <- c("#003366","#CC9966","black", "#FF9999","#339933")
V(net)$color <- colrs[V(net)$Habitat_cat] ##has to be numeric to assign colors this way

V(net)$Habitat_cat
V(net)$color

#Set edge width based on weight
E(net)$width <- E(net)$total_percap/5


plot(net, edge.arrow.size = 0.5, edge.curved = 0.5, vertex.label = V(net)$Resource)


##color edges of graph based on source node color
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- colrs[V(net)$Habitat_cat[edge.start]]

##want to create a matrix of coordinates, where y is the trophic level, and x is the habitat
tl$mean_TL <- as.numeric(tl$mean_TL)
lay <- matrix(nrow = nrow(tl), ncol = 2)
lay[,1] <- tl$Resource_cat
lay[,2] <- tl$mean_TL
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, arrow.size = 25, layout = lay)
fw_plot <- plot(net, edge.color = edge.col, edge.curved = 0, layout = lay, vertex.label = NA, vertex.frame.color = "white", edge.arrow.size = 2, asp = 0)


