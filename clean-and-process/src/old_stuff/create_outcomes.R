library(here)
source(here("functions.R"))

# include stats on the number of shift assignments which have a stop, arrest, use of force, etc.
# include updated stats after removing duplicates on number of officers, shifts, stops/arrests/uses of force
# check to see how many officers I retain (how many officers made stops, arrests, uses of force, etc.)

Create_Outcomes <- function(df, id_col) {
    
    flag <- df %>% mutate(count = if_else(is.na(.data[[id_col]]), 0, 1))
    
    total <- flag %>% count(shift_id, wt = count)
    
    # contact <-
    #     flag %>%
    #     count(shift_id, contact_type, wt = count) %>%
    #     filter(!is.na(contact_type)) %>%
    #     pivot_wider(id_cols = shift_id,
    #                 names_from = contact_type,
    #                 values_from = n,
    #                 values_fill = 0)
    
    race <-
        flag %>%
        count(shift_id, civ.race, wt = count) %>%
        filter(!is.na(civ.race)) %>%
        pivot_wider(id_cols = shift_id,
                    names_from = civ.race,
                    values_from = n,
                    values_fill = 0)
    
    # race_contact <-
    #     flag %>%
    #     count(shift_id, contact_type, civ.race, wt = count) %>%
    #     filter(!is.na(contact_type) & !is.na(civ.race)) %>%
    #     pivot_wider(id_cols = "shift_id",
    #                 names_from = c("contact_type", "civ.race"),
    #                 values_from = "n",
    #                 values_fill = 0)
    
    outcomes <-
        reduce(list(total, race), full_join, by = "shift_id") %>%
        mutate(across(-c("shift_id"), ~if_else(is.na(.x), 0, .x)))
    
    # outcomes <- reduce(list(total, contact, race, race_contact),
    #                    full_join, by = "shift_id")
}

stop_outcomes <- Create_Outcomes(stops_merged, "stop_officer_id")
colnames(stop_outcomes)[2:6] <- paste0("stops_", colnames(stop_outcomes))[2:6]

arrest_outcomes <- Create_Outcomes(arrests_merged, "arrest_officer_id")
colnames(arrest_outcomes)[2:6] <- paste0("arrests_", colnames(arrest_outcomes))[2:6]

force_outcomes <- Create_Outcomes(force_merged, "force_id")
colnames(force_outcomes)[2:6] <- paste0("force_", colnames(force_outcomes))[2:6]

assignments <-
    officer_assignments %>%
    mutate(rank = fct_collapse(rank,
                               leadership = c("CHIEF", "COMMANDER",
                                              "DEPUTY CHIEF", "LIEUTENANT")),
           rank = as.factor(tolower(fct_other(rank,
                                              keep = c("POLICE OFFICER",
                                                       "SERGEANT",
                                                       "leadership")))),
           officer_race = str_replace(officer_race, "officer_", "")) %>%
    select(shift_id, month, unit)

outcomes <-
    reduce(list(stop_outcomes, arrest_outcomes, force_outcomes, assignments),
           full_join, by = c("shift_id"))

write_csv(outcomes, here("clean-and-process", "src", "old_stuff", "assignments_with_outcomes.csv"))
