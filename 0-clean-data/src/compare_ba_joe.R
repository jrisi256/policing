library(here)
library(glue)
library(readr)
library(purrr)
library(dplyr)

ba <-
    read_csv(here("0-clean-data", "output", "stops-ba.csv")) %>%
    select(shift_id, matches("stops_n")) %>%
    rename(stops_n_civilian_hispanic = stops_n_civilian_hisp)

ba_joe <-
    read_csv(here("0-clean-data", "output", "stops-ba-joe.csv")) %>%
    select(shift_id, matches("stops_n"))

columns <- colnames(ba)[colnames(ba) != "shift_id"]

compare <-full_join(ba, ba_joe, by = "shift_id")

comparison <-
    map(columns, function(column, df) {
        
    df %>%
            mutate("check_{column}" :=
                       .data[[as.character(glue("{column}.x"))]] ==
                       .data[[as.character(glue("{column}.y"))]]) %>%
            select(shift_id, glue("check_{column}"))
        
}, df = compare)

a <- reduce(comparison, full_join, by = "shift_id")
