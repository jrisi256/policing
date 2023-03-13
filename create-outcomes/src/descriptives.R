library(here)
library(readr)
library(stargazer)
library(modelsummary)

shift_assignments <-
    my_read_csv(here("create-outcomes",
                     "input",
                     "officers_assignments_ba.csv")) %>%
    select(officer_id, month, unit, date, shift, weekday, months_from_start,
           months_from_start_sq, shift_id, officer_race, officer_gender,
           spanish, beat_assigned)

unique_officers <-
    distinct(shift_assignments, officer_id, officer_race, officer_gender,
             months_from_start) %>%
    group_by(officer_id) %>%
    filter(months_from_start == max(months_from_start)) %>%
    ungroup()
    
