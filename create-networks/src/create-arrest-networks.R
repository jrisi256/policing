library(here)
source(here("functions.R"))

arrests <-
    my_read_csv(here("merge-arrests-shifts",
                     "output",
                     "arrests_officers_assignments_risi_min.csv"))

arrests_edgelist_undirected <- CreateEdgelist(arrests, "arrest_id")

write_csv(arrests_edgelist_undirected,
          here("create-networks", "output", "arrests-undirected-edgelist.csv"))
