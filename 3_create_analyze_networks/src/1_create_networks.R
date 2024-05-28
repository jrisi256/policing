library(here)
library(dplyr)
library(readr)
library(tidyr)
library(dtplyr)
library(ggraph)
library(stringr)
library(tidygraph)
out_path <- here("2_clean_data", "output")

##################################################################
##                         Read in data                         ##
##################################################################
stop_assignments <-
    read_csv(
        file.path(out_path, "3_stops_assignments_risi.csv.gz"),
        col_types = cols_only(
            stop_id = "c",
            officer_id = "c",
            officer_stop_id = "c",
            officer_race = "c",
            officer_sex = "c",
            birth_year = "d",
            appointed_month = col_date(format = "%Y-%m-%d"),
            spanish = "l",
            time = col_datetime(),
            district = "c",
            stop_type = "c",
            contact_type = "c",
            civ.race = "c",
            civ.sex = "c",
            civ.age = "d",
            lat = "d",
            lon = "d",
            unit = "c",
            beat_assigned = "c",
            po_first = "d",
            shift = "c"
        )
    )

arrest_assignments <-
    read_csv(
        file.path(out_path, "3_arrests_assignments_risi.csv.gz"),
        col_types = cols_only(
            arrest_id = "c",
            officer_id = "c",
            officer_arrest_id = "c",
            officer_race = "c",
            officer_sex = "c",
            birth_year = "d",
            appointed_month = col_date(format = "%Y-%m-%d"),
            spanish = "l",
            time = col_datetime(),
            district = "c",
            crime_code = "c",
            statute_description = "c",
            civ.race = "c",
            civ.sex = "c",
            civ.age = "d",
            lat = "d",
            lon = "d",
            unit = "c",
            beat_assigned = "c",
            shift = "c"
        )
    )

stops <-
    read_csv(
        file.path(out_path, "cleaned_stops.csv.gz"),
        col_types =
            cols_only(
                stop_id = "c",
                time = col_datetime(),
                district = "c",
                po_first = "d",
                stop_type = "c",
                contact_type = "c",
                civ.race = "c",
                civ.sex = "c",
                civ.age = "d",
                lat = "d",
                lon = "d",
                officer_id = "c",
                officer_stop_id = "c"
            )
    )

arrests <-
    read_csv(
        file.path(out_path, "cleaned_arrests.csv.gz"),
        col_types = 
            cols_only(
                arrest_id = "c",
                time = col_datetime(),
                district = "c",
                crime_code = "c",
                statute_description = "c",
                civ.race = "c",
                civ.sex = "c",
                civ.age = "d",
                lat = "d",
                lon = "d",
                officer_id = "c",
                officer_arrest_id = "c"
            )
    )

##################################################################
##                Create officer node data frame                ##
##################################################################
# Make sure officer_id is the first column for the network.
officer_nodes <-
    read_csv(
        file.path(out_path, "cleaned_officers.csv.gz"),
        col_types = cols(officer_id = "c")
    ) %>%
    relocate(officer_id)

stop_assignments_all <-
    stops %>%
    left_join(officer_nodes, by = "officer_id") %>%
    # Keep only those stops which did not match to an officer's work assignment.
    filter(!(officer_stop_id) %in% stop_assignments$officer_stop_id) %>%
    # Then merge with stops which did match to a work assignment.
    bind_rows(stop_assignments) %>%
    mutate(
        unit = if_else(is.na(unit), "NO MATCH", unit),
        beat_assigned = if_else(is.na(beat_assigned), "NO MATCH", beat_assigned),
        shift = if_else(is.na(shift), "NO MATCH", shift)
    )
    
arrest_assignments_all <-
    arrests %>%
    left_join(officer_nodes, by = "officer_id") %>%
    # Keep only those arrests which did not match to an officer's work assignment.
    filter(!(officer_arrest_id) %in% arrest_assignments$officer_arrest_id) %>%
    # then marge with arrests which did match to a work assignment.
    bind_rows(arrests_assignments) %>%
    mutate(
        unit = if_else(is.na(unit), "NO MATCH", unit),
        beat_assigned = if_else(is.na(beat_assigned), "NO MATCH", beat_assigned),
        shift = if_else(is.na(shift), "NO MATCH", shift)
    )

##################################################################
##      Create edge list data frames for stops and arrests      ##
##################################################################
stop_edgelist_df <-
    stop_assignments_all %>%
    # Remove officer work assignments with no stops.
    filter(!is.na(stop_id)) %>%
    # Create pairs of officers who made stops together.
    full_join(
        select(
            stop_assignments_all, officer_id, unit, beat_assigned, po_first,
            shift, officer_race, officer_race, spanish, officer_sex, birth_year,
            appointed_month, stop_id
        ),
        by = "stop_id",
        suffix = c("_1", "_2"),
        multiple = "all"
    ) %>%
    # Remove those pairs which just match an officer to themselves.
    filter(officer_id_1 != officer_id_2) %>%
    # Create a unique id for each officer pairing (1-2 and 2-1 are the same).
    rowwise() %>%
    mutate(
        officer_pair_id =
            paste0(
                sort(c(officer_id_1, officer_id_2))[1],
                "_",
                sort(c(officer_id_1, officer_id_2))[2]
            )
    ) %>%
    distinct(stop_id, officer_pair_id, .keep_all = T) %>%
    select(-officer_pair_id) %>%
    # Check and see which officers were working together when they made a stop.
    mutate(
        beat_no_alpha_1 = 
            if_else(
                beat_assigned_1 != "NO MATCH",
                str_replace_all(beat_assigned_1, "[A-Za-z]", ""),
                beat_assigned_1
            ),
        beat_no_alpha_2 = 
            if_else(
                beat_assigned_2 != "NO MATCH",
                str_replace_all(beat_assigned_2, "[A-Za-z]", ""),
                beat_assigned_2
            ),
        shared_beat_no_alpha = beat_no_alpha_1 == beat_no_alpha_2,
        shared_beat_unit_shift =
            beat_assigned_1 == beat_assigned_2 & unit_1 == unit_2 & shift_1 == shift_2,
        shared_unit = unit_1 == unit_2,
        # Can use which officer initiated the stop to create a directed graph
        from = if_else(po_first_1 == 1, officer_id_1, officer_id_2),
        to = if_else(po_first_1 == 1, officer_id_2, officer_id_1)
    ) %>%
    relocate(from, to)

b <- stop_edges %>% filter(shared_beat_unit_shift & (unit_officer_1 != "NO MATCH" & unit_officer_2 != "NO MATCH"))

a <- stop_edges %>% filter(shared_beat_unit_shift & unit_officer_1 != "NO MATCH" & unit_officer_2 != "NO MATCH")
a2 <- stop_edges %>% filter(unit_officer_1 == "NO MATCH" | unit_officer_2 == "NO MATCH" | !shared_beat_unit_shift)
a3 <- stop_edges %>% filter(!shared_beat_unit_shift & unit_officer_1 != "NO MATCH" & unit_officer_2 != "NO MATCH")

prop.table(table(a$officer_race_officer_1 == a$officer_race_officer_2, useNA = "ifany"))
prop.table(table(a$officer_sex_officer_1 == a$officer_sex_officer_2, useNA = "ifany"))
prop.table(table(a$spanish_officer_1 == a$spanish_officer_2, useNA = "ifany"))
summary(abs(a$birth_year_officer_1 - a$birth_year_officer_2))

prop.table(table(stop_edges$officer_race_officer_1 == stop_edges$officer_race_officer_2, useNA = "ifany"))
prop.table(table(stop_edges$officer_sex_officer_1 == stop_edges$officer_sex_officer_2, useNA = "ifany"))
prop.table(table(stop_edges$spanish_officer_1 == stop_edges$spanish_officer_2, useNA = "ifany"))
summary(abs(stop_edges$birth_year_officer_1 - stop_edges$birth_year_officer_2))

prop.table(table(a2$officer_race_officer_1 == a2$officer_race_officer_2, useNA = "ifany"))
prop.table(table(a2$officer_sex_officer_1 == a2$officer_sex_officer_2, useNA = "ifany"))
prop.table(table(a2$spanish_officer_1 == a2$spanish_officer_2, useNA = "ifany"))
summary(abs(a2$birth_year_officer_1 - a2$birth_year_officer_2))

prop.table(table(a3$officer_race_officer_1 == a3$officer_race_officer_2, useNA = "ifany"))
prop.table(table(a3$officer_sex_officer_1 == a3$officer_sex_officer_2, useNA = "ifany"))
prop.table(table(a3$spanish_officer_1 == a3$spanish_officer_2, useNA = "ifany"))
summary(abs(a3$birth_year_officer_1 - a3$birth_year_officer_2))

##################################################################
##                     Create stops network                     ##
##################################################################
stop_edges_directed <-
    lazy_dt() %>%
    stop_edges %>%
    count(from, to) %>%
    as_tibble()

stop_edges_undirected <-
    stop_edges %>%
    rowwise() %>%
    mutate(
        undirected_from = 
            sort(c(from, to))[1],
        undirected_to = sort(c(from, to))[2]
    ) %>%
    lazy_dt() %>%
    count(undirected_from, undirected_to, name = "nr_stops_together") %>%
    as_tibble()

stop_network_undirected <-
    tbl_graph(nodes = officer_nodes, edges = stop_edges_undirected, directed = F) %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(weights = nr_stops_together)) %>%
    filter(degree >= 500)

##################################################################
##                      Graph stop network                      ##
##################################################################
ggraph(stop_network_undirected, layout = "fr") +
    geom_edge_link(edge_alpha = 0.25, edge_color = "black") +
    geom_node_point(aes(color = officer_race)) +
    theme_void()

#######################################################################
##  Graph network of officers who make stops while working together  ##
#######################################################################
stop_edges_undirected_work_together <-
    stop_edges %>%
    filter(shared_beat_no_alpha) %>%
    rowwise() %>%
    mutate(
        undirected_from = sort(c(from, to))[1],
        undirected_to = sort(c(from, to))[2]
    ) %>%
    lazy_dt() %>%
    count(undirected_from, undirected_to, name = "nr_stops_together") %>%
    as_tibble()

stop_edges_undirected_no_work_together <-
    stop_edges %>%
    filter(!shared_beat_no_alpha) %>%
    rowwise() %>%
    mutate(
        undirected_from = sort(c(from, to))[1],
        undirected_to = sort(c(from, to))[2]
    ) %>%
    lazy_dt() %>%
    count(undirected_from, undirected_to, name = "nr_stops_together") %>%
    as_tibble()

stop_network_undirected_work_together <-
    tbl_graph(nodes = officer_nodes, edges = stop_edges_undirected_work_together, directed = F) %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(weights = nr_stops_together)) %>%
    filter(degree >= 350)

stop_network_undirected_no_work_together <-
    tbl_graph(nodes = officer_nodes, edges = stop_edges_undirected, directed = F) %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(weights = nr_stops_together)) %>%
    filter(degree >= 350)

ggraph(stop_network_undirected_work_together, layout = "fr") +
    geom_edge_link(edge_alpha = 0.25, edge_color = "black") +
    geom_node_point(aes(color = officer_race)) +
    theme_void() +
    labs(title = "Officers who made stops and were working together")

ggraph(stop_network_undirected_no_work_together, layout = "fr") +
    geom_edge_link(edge_alpha = 0.25, edge_color = "black") +
    geom_node_point(aes(color = officer_race)) +
    theme_void() +
    labs(title = "Officers who made stops and were not working together")
