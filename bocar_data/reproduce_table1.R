library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(data.table)

################################################# Read in officer behavior
assignmentOfficer <- read_csv(here("bocar_data", "officerAssignment.csv"))
stops <- read_csv(here("bocar_data", "stops.csv"))
# arrests <- read_csv(here("bocar_data", "arrests.csv"))
# force <- read_csv(here("bocar_data", "force.csv"))

# Stops, using only the first police officer
# stops.1 <- stops %>% filter(po_first == 1)

###################################### Merge stops and shift assignments
assignmentOfficer <-
    assignmentOfficer %>%
    arrange(officer_id, date) %>%
    rename(assignment_date = date) %>%
    mutate(oa_id = row_number())

stops <-
    stops %>%
    arrange(officer_id, date) %>%
    select(-civilian_race_short, -month) %>%
    rename(stop_date = date) %>%
    mutate(os_id = row_number())

# Merge stops and shift assignments
stopsMerged <-
    right_join(stops,
               assignmentOfficer,
               by = c("officer_id", "stop_date" =  "assignment_date")) %>%
    rename(date = stop_date) %>%
    filter(is.na(hour) | between(hour, floor(start_time), ceiling(end_time)))

# Find stops which happened during multi-day shifts
assignmentsNextDay <-
    assignmentOfficer %>% 
    filter(end_time > 24) %>%
    mutate(start_time = 0, end_time = end_time - 24,
           date_nextday = assignment_date + 1) %>%
    arrange(officer_id, date_nextday)

stopsMergedNextDay <-
    right_join(stops,
               assignmentsNextDay,
               by = c("officer_id", "stop_date" = "date_nextday")) %>%
    filter(is.na(hour) | between(hour, floor(start_time), ceiling(end_time))) %>%
    mutate(date = assignment_date, hour = hour + 24, end_time = end_time + 24,
           start_time = end_time - duration) %>%
    select(-stop_date, -assignment_date)

stopsMergedFinal <-
    bind_rows(stopsMerged, stopsMergedNextDay) %>%
    distinct() %>%
    mutate(final_id = paste0(os_id, " ", oa_id))

stopsByGroup <-
    stopsMergedFinal %>%
    mutate(stopCount = if_else(is.na(stop_id), 0 , 1)) %>%
    count(officer_id, beat_assigned, shift, date, wt = stopCount)

stopsByGroupType <-
    stopsMergedFinal %>%
    mutate(stopCount = 1) %>%
    count(officer_id, beat_assigned, shift, date, contact_type, wt = stopCount) %>%
    pivot_wider(names_from = contact_type, values_from = n, values_fill = 0)

stopsByCivRace <-
    stopsMergedFinal %>%
    mutate(stopCount = 1) %>%
    count(officer_id, beat_assigned, shift, date, civilian_race_short, wt = stopCount) %>%
    pivot_wider(names_from = civilian_race_short, values_from = n, values_fill = 0)

assignmentsStops <-
    stopsByGroup %>%
    right_join(assignmentOfficer, by = c("officer_id", "beat_assigned", "shift", "date"))
nrow(stopsMergedFinal %>% filter(!is.na(stop_id)))
sum(assignmentsStops[, "n"], na.rm = TRUE)

fltr <- stopsMergedFinal %>% filter(!is.na(stop_id))
table(fltr$officer_race)
table(fltr$officer_gender)
