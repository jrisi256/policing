library(here)
source(here("functions.R"))

stops_assignments <-
    read_csv(here("New", "linda_work",
                     "stops_officers_assignments_ba_max.csv"))

arrests_assignments <-
    my_read_csv(here("create-outcomes",
                     "input",
                     "arrests_officers_assignments_ba_max.csv")) %>%
    mutate(domestic =
               case_when(
                   str_detect(statute_description, "DOMESTIC") & crime_code == "violent" ~ "domestic",
                   !str_detect(statute_description, "DOMESTIC") & crime_code == "violent" ~ "non_domestic",
                   T ~ "non_violent"))

force_assignments <-
    my_read_csv(here("create-outcomes",
                     "input",
                     "force_officers_assignments_ba_max.csv"))

stop_count <- Create_Outcomes(stops_assignments, "stops", stop_id)
stop_type <- Create_Outcomes(stops_assignments, "stops", stop_id, contact_type)
stop_race <- Create_Outcomes(stops_assignments, "stops", stop_id, civ.race)
stop_type_race <- Create_Outcomes(stops_assignments, "stops", stop_id, contact_type, civ.race)
stop_outcomes <-
    reduce(list(stop_count, stop_type, stop_race), full_join, by = "shift_id") %>%
    mutate(across(-shift_id, ~if_else(is.na(.x), 0, .x)))

arrest_count <- Create_Outcomes(arrests_assignments, "arrests", arrest_id)
arrest_type <- Create_Outcomes(arrests_assignments, "arrests", arrest_id, crime_code)
arrest_type_domestic <- Create_Outcomes(arrests_assignments, "arrests", arrest_id, domestic)
arrest_race <- Create_Outcomes(arrests_assignments, "arrests", arrest_id, civ.race)
arrest_outcomes <-
    reduce(list(arrest_count, arrest_type, arrest_race, arrest_type_domestic), full_join, by = "shift_id") %>%
    mutate(across(-shift_id, ~if_else(is.na(.x), 0, .x)))

force_count <- Create_Outcomes(force_assignments, "force", force_id)
force_type <- Create_Outcomes(force_assignments, "force_injured", force_id, civ.injured)
force_race <- Create_Outcomes(force_assignments, "force", force_id, civ.race)
force_outcomes <-
    reduce(list(force_count, force_type, force_race), full_join, by = "shift_id") %>%
    mutate(across(-shift_id, ~if_else(is.na(.x), 0, .x)))

outcomes <- reduce(list(stop_outcomes, arrest_outcomes, force_outcomes),
                   full_join,
                   by = "shift_id")

write_csv(outcomes, here("create-outcomes", "output", "outcomes_raw_ba_max.csv"))
