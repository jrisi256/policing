library(here)
library(assortnet)
source(here("functions.R"))

officers <-
    my_read_csv(here("create-networks", "output", "officers.csv")) %>%
    mutate(age = 2015 - birth_year,
           exp = (ymd("2015-12-01") - appointed_month) / dyears(1))

ggplot(officers, aes(x = officer_race)) + geom_bar() + theme_bw()
ggplot(officers, aes(x = officer_sex)) + geom_bar() + theme_bw()
ggplot(officers, aes(x = spanish)) + geom_bar() + theme_bw()

ggplot(officers, aes(x = age)) +
    geom_histogram(bins = 25) +
    theme_bw() +
    geom_vline(aes(xintercept = median(age)),
               color = "blue")

ggplot(officers, aes(x = exp)) +
    geom_histogram(bins = 25) +
    theme_bw()+
    geom_vline(aes(xintercept = median(exp)),
           color = "blue")

stops_edgelist <-
    my_read_csv(here("create-networks",
                     "output",
                     "stops-directed-edgelist.csv")) %>%
    count(officer_id.x, officer_id.y)

summary(stops_edgelist$n)
ggplot(stops_edgelist, aes(x = n)) + geom_histogram(bins = 40) + theme_bw()

stops_edgelist_noloops <-
    stops_edgelist %>%
    filter(officer_id.x != officer_id.y)

summary(stops_edgelist_noloops$n)

ggplot(stops_edgelist_noloops, aes(x = n)) +
    geom_histogram(bins = 40) +
    theme_bw()

stops_net <-
    graph_from_data_frame(d = stops_edgelist_noloops,
                          vertices = officers,
                          directed = F)
E(stops_net)$weight <- stops_edgelist_noloops$n

V(stops_net)$size <- degree(stops_net) / 10
E(stops_net)$width <- E(stops_net)$n / 50
E(stops_net)$arrow.size <- 0
plot(stops_net, vertex.label = NA)

stops_top <-
    stops_edgelist_noloops %>%
    filter(n > 27)

stops_bottom <-
    stops_edgelist_noloops %>%
    filter(n <= 4)

stops_top_net <-
    graph_from_data_frame(d = stops_top, vertices = officers, directed = F)

stops_bottom_net <-
    graph_from_data_frame(d = stops_bottom, vertices = officers, directed = F)

V(stops_top_net)$size <- degree(stops_top_net) / 1.5
E(stops_top_net)$width <- E(stops_top_net)$n / 30
E(stops_top_net)$arrow.size <- 0
plot(stops_top_net, vertex.label = NA)

V(stops_bottom_net)$size <- degree(stops_bottom_net) / 10
E(stops_bottom_net)$width <- E(stops_bottom_net)$n / 5
E(stops_bottom_net)$arrow.size <- 0
plot(stops_bottom_net, vertex.label = NA)

assortativity.nominal(stops_net,
                      as.integer(as.factor(V(stops_net)$officer_race)),
                      directed = F)
assortativity.nominal(stops_top_net,
                      as.integer(as.factor(V(stops_top_net)$officer_race)),
                      directed = F)
assortativity.nominal(stops_bottom_net,
                      as.integer(as.factor(V(stops_bottom_net)$officer_race)),
                      directed = F)

assortativity.nominal(stops_net,
                      as.integer(as.factor(V(stops_net)$officer_sex)),
                      directed = F)
assortativity.nominal(stops_top_net,
                      as.integer(as.factor(V(stops_top_net)$officer_sex)),
                      directed = F)
assortativity.nominal(stops_bottom_net,
                      as.integer(as.factor(V(stops_bottom_net)$officer_sex)),
                      directed = F)

assortativity(stops_net, V(stops_net)$age, directed = F)
assortativity(stops_top_net, V(stops_top_net)$age, directed = F)
assortativity(stops_bottom_net, V(stops_bottom_net)$age, directed = F)

assortativity(stops_net, V(stops_net)$exp, directed = F)
assortativity(stops_top_net, V(stops_top_net)$exp, directed = F)
assortativity(stops_bottom_net, V(stops_bottom_net)$exp, directed = F)

assortativity_degree(stops_net, directed = F)
assortativity_degree(stops_top_net, directed = F)
assortativity_degree(stops_bottom_net, directed = F)

assortativity(stops_net, graph.strength(stops_net), directed = F)
assortativity(stops_top_net, graph.strength(stops_top_net), directed = F)
assortativity(stops_bottom_net, graph.strength(stops_bottom_net), directed = F)

race_pairs <-
    stops_edgelist_noloops %>%
    inner_join(select(officers, officer_id, officer_race),
               by = c("officer_id.x" = "officer_id")) %>%
    inner_join(select(officers, officer_id, officer_race),
               by = c("officer_id.y" = "officer_id"))

mm <- prop.table(table(race_pairs$officer_race.x, race_pairs$officer_race.y))
(sum(diag(mm)) - sum(colSums(mm) * rowSums(mm))) / (1 - sum(colSums(mm) * rowSums(mm)))

assortment.discrete(as_adjacency_matrix(stops_net, attr = "weight"),
                    as.integer(as.factor(V(stops_net)$officer_race)),
                    weighted = F)

assortment.discrete(as_adjacency_matrix(stops_net, attr = "weight"),
                    as.integer(as.factor(V(stops_net)$officer_race)))

assortment.discrete(as_adjacency_matrix(stops_net),
                    as.integer(as.factor(V(stops_net)$officer_race)))
