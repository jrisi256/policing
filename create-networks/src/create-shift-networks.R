library(here)
source(here("functions.R"))

officer_assignments <-
    my_read_csv(here("create-networks",
                     "input",
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
    select(shift_id, beat_shift_id, officer_id)

beat_edgelist <- beat_shifts %>% CreateEdgelist("beat_shift_id")

unit_shifts <-
    officer_assignments %>%
    distinct(unit, shift, date) %>%
    mutate(unit_shift_id = row_number()) %>%
    full_join(officer_assignments) %>%
    select(shift_id, unit_shift_id, officer_id)
    
unit_edgelist <- unit_shifts %>% CreateEdgelist("unit_shift_id")
