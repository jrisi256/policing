library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(data.table)

################################################# Read in officer behavior
ao <- read_csv(here("bocar_data", "officerAssignment.csv"))
ss <- read_csv(here("bocar_data", "stops.csv"))

###################################### Clean up officer assignments and stops
ao <-
    ao %>%
    arrange(officer_id, date) %>%
    select(-rank) %>%
    mutate(oa_id = row_number()) %>%
    rename(assignment_date = date)

ss <-
    ss %>%
    arrange(officer_id, date) %>%
    select(-civilian_race_short, -month) %>%
    mutate(os_id = row_number()) %>%
    rename(stop_date = date)

# Number of unique stops
length(unique(ss$stop_id))

# Create a start datetime and an end datetime 
ao <-
    ao %>%
    mutate(start_assignment_date = if_else(is.na(start_time),
                                           NA_Date_,
                                           assignment_date),
           start_hms = hms::hms(rep(0, nrow(ao)), rep(0, nrow(ao)), start_time),
           start_datetime = as_datetime(paste(start_assignment_date, " ", start_hms)),
           end_assignment_date = if_else(end_time > 24,
                                         assignment_date + 1,
                                         assignment_date),
           end_hms = hms::hms(rep(0, nrow(ao)),
                              rep(0, nrow(ao)),
                              if_else(end_time > 24, end_time - 24, end_time)),
           end_datetime = as_datetime(paste(end_assignment_date, " ", end_hms)))

mergeSameDay <-
    right_join(ss, ao, by = c("officer_id", "stop_date" = "assignment_date")) %>%
    filter(is.na(hour) | between(floor_date(time, unit = "hour"),
                                 floor_date(start_datetime, unit = "hour"),
                                 ceiling_date(end_datetime, unit = "hour")))

aoNextDay <-
    ao %>%
    filter(end_time > 24) %>% # why is this a thing
    mutate(assignment_date = assignment_date + 1)

mergeNextDay <-
    right_join(ss, aoNextDay, by = c("officer_id", "stop_date" = "assignment_date")) %>%
    filter(is.na(hour) | between(floor_date(time, unit = "hour"),
                                 floor_date(start_datetime, unit = "hour"),
                                 ceiling_date(end_datetime, unit = "hour"))) %>%
    mutate(stop_date = stop_date - 1)

mergeFinal <-
    bind_rows(mergeSameDay, mergeNextDay) %>%
    distinct() %>%
    mutate(final_id = paste0(os_id, " ", oa_id))
fltrMerge <- mergeFinal %>% filter(!is.na(stop_id))

# Check missings on Bocar data
a <- fltrMerge %>% filter(!(final_id %in% fltr$final_id))
a2 <- fltr %>% filter(!(final_id %in% fltrMerge$final_id))
a3 <- fltr %>% filter(is.na(start_time))

length(unique(mergeFinal$final_id))
length(unique(stopsMergedFinal$final_id))
