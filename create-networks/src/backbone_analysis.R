library(here)
library(backbone)
source(here("functions.R"))

arrests_edgelist <-
    my_read_csv(here("create-networks",
                     "output",
                     "arrests-undirected-edgelist.csv"))

arrests_edgelist_weighted <-
    arrests_edgelist %>%
    count(officer_id.x, officer_id.y) %>%
    filter(officer_id.x != officer_id.y)

officers <- my_read_csv(here('create-networks', 'output', 'officers.csv'))

arrests_net <- graph_from_data_frame(d = arrests_edgelist_weighted,
                                     vertices = officers,
                                     directed = F)
isolated_nodes <- which(degree(arrests_net) == 0)
arrests_net <- delete.vertices(arrests_net, isolated_nodes)
E(arrests_net)$weight <- arrests_edgelist_weighted$n
V(arrests_net)$size <- degree(arrests_net) / 5
E(arrests_net)$width <- E(arrests_net)$n / 10
E(arrests_net)$arrow.size <- 0
plot(arrests_net, vertex.label = NA)

arrests_net_backbone <- disparity(arrests_net, narrative = T)
V(arrests_net_backbone)$size <- degree(arrests_net_backbone) / 1.5
E(arrests_net_backbone)$width <- 3
E(arrests_net_backbone)$arrow.size <- 0
plot(arrests_net_backbone, vertex.label = NA)

isolated_nodes <- which(degree(arrests_net_backbone) == 0)
backbone_full <- delete.vertices(arrests_net_backbone, isolated_nodes)
V(backbone_full)$size <- degree(backbone_full) / 1.25
E(backbone_full)$width <- 6
E(backbone_full)$arrow.size <- 0
plot(backbone_full, vertex.label = NA)
