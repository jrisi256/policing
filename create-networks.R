library(here)
source(here("functions.R"))

CreateNetwork <- function(df, id_col) {
    
    df %>%
        filter(!is.na(.data[[id_col]])) %>%
        group_by(.data[[id_col]]) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        select(all_of(id_col), officer_id) %>%
        inner_join(select(df, all_of(id_col), officer_id), by = id_col) %>%
        filter(officer_id.x != officer_id.y) %>%
        group_by(across(everything())) %>%
        mutate(pair = paste0(min(officer_id.x, officer_id.y),
                             ";",
                             max(officer_id.x, officer_id.y))) %>%
        ungroup() %>%
        distinct(.data[[id_col]], pair, .keep_all = T) %>%
        select(-all_of(id_col), -pair) %>%
        count(officer_id.x, officer_id.y)
}

stops <-
    my_read_csv(here("merge-stops-shifts",
                     "output",
                     "stops_officers_assignments_risi_min.csv"))

arrests <-
    my_read_csv(here("merge-arrests-shifts",
                     "output",
                     "arrests_officers_assignments_risi_min.csv"))

<<<<<<< HEAD
stop_network <- CreateNetwork(stops, "stop_id")
arrest_network <- CreateNetwork(arrests, "arrest_id")
=======
arrest_network <-
    arrests %>%
    filter(!is.na(arrest_id)) %>%
    group_by(arrest_id) %>%
    filter(n() > 1) %>%
    ungroup()
>>>>>>> ca884af120f9b09f6663daab6801007165914c5c

#1184685
a <-
    arrest_network %>%
    select(arrest_id, officer_id) %>%
    inner_join(select(arrest_network, arrest_id, officer_id),
               by = "arrest_id") %>%
    filter(officer_id.x != officer_id.y) %>%
    group_by(arrest_id, officer_id.x, officer_id.y) %>%
    mutate(pair = paste0(min(officer_id.x, officer_id.y),
                         "; ",
                         max(officer_id.x, officer_id.y))) %>%
    ungroup() %>%
    distinct(arrest_id, pair, .keep_all = T) %>%
    select(-arrest_id, -pair) %>%
    count(officer_id.x, officer_id.y)

net <- graph_from_data_frame(d = a, directed = F)
V(net)$size <- 1

l <- layout_with_fr(net)
plot(net, vertex.label = NA)
plot(net, vertex.label = NA, layout = l)
plot(net, vertex.label = NA, layout = layout_with_fr)

net_del <- delete_edges(net, E(net)[n < mean(a$n)])
V(net_del)$size <- 1
E(net_del)$width <- sqrt(E(net_del)$n)

plot(net_del, vertex.label = NA)

nodes <- read_csv(paste0("~/Documents/schoolwork/plsc508/", "class2/",
                       "sunbelt2021/",
                       "Data files/",
                       "Dataset1-Media-Example-NODES.csv"))

links <- read_csv(here("~/Documents/schoolwork/plsc508/", "class2/",
                       "sunbelt2021/",
                       "Data files/",
                       "Dataset1-Media-Example-EDGES.csv"))

