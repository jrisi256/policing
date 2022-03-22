library(here)
library(glue)
library(readr)
library(purrr)
library(dplyr)

# Read in data
ba <-
    read_csv(here("0-clean-data", "output", "stops-outcomes_ba.csv")) %>%
    select(shift_id, matches("stops_n")) %>%
    rename(stops_n_civilian_hispanic = stops_n_civilian_hisp)

ba_joe <-
    read_csv(here("0-clean-data", "output", "stops-outcomes_ba-joe.csv")) %>%
    select(shift_id, matches("stops_n"))

# Specify columns for comparison
columns <- colnames(ba)[colnames(ba) != "shift_id"]

# Join the different data sets
compare_df <- full_join(ba, ba_joe, by = "shift_id")

# Compare the columns for equality
comparison <-
    map(columns, function(column, df) {
        
    df %>%
            mutate("check_{column}" :=
                       .data[[as.character(glue("{column}.x"))]] ==
                       .data[[as.character(glue("{column}.y"))]]) %>%
            select(shift_id, glue("check_{column}"))
        
}, df = compare_df)

# Create the resulting data frame from the mapped comparisons
results_df <- reduce(comparison, full_join, by = "shift_id") %>% select(-shift_id)
results_df <-
    pmap_dfr(list(results_df, names(results_df)), function(col, col_name) {
        table(col, useNA = 'ifany') %>%
            as_tibble() %>%
            mutate(column = col_name)})

write_csv(results_df, file = here("0-clean-data", "output", "stops_ba-joe_check.csv"))
