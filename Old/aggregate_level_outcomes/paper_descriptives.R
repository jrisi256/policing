library(dplyr)
library(readr)

shifts_with_stops <-
    stops_officers_assignments_ba_max %>%
    filter(!is.na(stop_id))

shifts_with_stops_nr <- length(unique(shifts_with_stops$shift_id))
total_shifts <- length(unique(stops_officers_assignments_ba_max$shift_id))

shifts_with_arrests <-
    arrests_officers_assignments_ba_max %>%
    filter(!(is.na(arrest_id)))

shift_arrests_nr <- length(unique(shifts_with_arrests$shift_id))
total_shifts <- length(unique(arrests_officers_assignments_ba_max$shift_id))

shifts_with_force <-
    force_officers_assignments_ba_max %>%
    filter(!is.na(force_id))

shift_force_nr <- length(unique(shifts_with_force$shift_id))    
total_shifts <- length(unique(force_officers_assignments_ba_max$shift_id))
