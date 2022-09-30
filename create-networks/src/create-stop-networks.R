library(here)
source(here("functions.R"))

stops <-
    my_read_csv(here("merge-stops-shifts",
                     "output",
                     "stops_officers_assignments_risi_min.csv"))

stops_edgelist_directed <- CreateEdgelist(stops, "stop_id", "po_first")

write_csv(stops_edgelist_directed,
          here("create-networks", "output", "stops-directed-edgelist.csv"))
