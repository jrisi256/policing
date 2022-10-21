library(here)
source(here("functions.R"))

officer_assignments <-
    my_read_csv(here("create-officer-assignments",
                     "output",
                     "officers_assignments_risi.csv"))

officers <-
    officer_assignments %>%
    select(officer_id, birth_year, appointed_month, officer_race, officer_sex,
           spanish) %>%
    distinct()

write_csv(officers, here("create-networks", "output", "officers.csv"))

beat_shifts <-
    officer_assignments %>%
    distinct(unit, shift, date, beat_assigned) %>%
    mutate(beat_shift_id = row_number()) %>%
    full_join(officer_assignments) %>%
    select(shift_id, officer_id, beat_shift_id)

beat_edgelist <- beat_shifts %>% CreateEdgelist("beat_shift_id")

unit_shifts <-
    officer_assignments %>%
    distinct(unit, shift, date) %>%
    mutate(unit_shift_id = paste0(row_number(), "a")) %>%
    full_join(officer_assignments) %>%
    select(officer_id, unit_shift_id)

net <- graph_from_data_frame(unit_shifts, directed = F)
V(net)$type <- V(net)$name %in% unit_shifts$unit_shift_id
net_bp <- bipartite_projection(net, which = F)
E(net_bp)$width <- 0.05
V(net_bp)$size <- degree(net_bp) / 60
E(net_bp)$arrow.size <- 0
plot(net_bp, vertex.label = NA)

unit_shifts_edge <-
    unit_shifts %>%
    select(-shift_id) %>%
    mutate(dummy = 1,
           unit_shift_id = paste0("shift_", unit_shift_id)) %>%
    pivot_wider(names_from = unit_shift_id,
                values_from = dummy,
                values_fill = 0)
    
unit_edgelist <- unit_shifts %>% CreateEdgelist("unit_shift_id")

