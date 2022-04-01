library(here)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)

`%.%` <- paste0

# officer assignments
assignments <-
    fread(here("0-clean-data", "output", "officer_assignments-ba.csv")) %>%
    mutate(date = ymd(date))

# stops
stops <-
    fread(here("0-clean-data", "input", "bocar-ba_data", 'stops.csv')) %>%
    mutate(stop_officer_id = row_number())
stops[, date := ymd(date)]

# sort for merging
setkey(assignments, officer_id, date)
setkey(stops, officer_id, date)

# subset to stops occurring during recorded shift
## step 1: match officer_id, match stop date to date of shift start
stops.merged <- stops[assignments]

## drop if not during shift
stops.merged <-
    stops.merged[  
    is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),]

## step 2: match officer_id, deal with stops past midnight on overnight shifts
assignments.nextday <- assignments[end_time > 24]  # find overnight shifts
assignments.nextday[, start_time := 0]
assignments.nextday[, end_time := end_time - 24]
assignments.nextday[, date_nextday := date] # copy date...
day(assignments.nextday$date_nextday) <-
    day(assignments.nextday$date_nextday) + 1 # ... and increment forward
setkey(assignments.nextday, officer_id, date_nextday)

stops.merged.nextday <-
    stops[assignments.nextday,,
          on = c(officer_id = 'officer_id', date = 'date_nextday')]

## drop if not during shift
stops.merged.nextday <-
    stops.merged.nextday[
        is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),]
stops.merged.nextday[, date := i.date]
stops.merged.nextday[, i.date := NULL]
stops.merged.nextday[, hour := hour + 24]

## merge same-day stops with next-day stops on overnight shifts
stops.merged <- rbind(stops.merged, stops.merged.nextday)
stops.merged <- unique(stops.merged)

# tabulate stops by group (officer x period)
## including broken down by various stop types (reason, suspect race)
stops.by.group <-
    stops.merged[, .(stops_n_total = sum(!is.na(stop_id))), by = c('shift_id')]

## counts by contact type
stops.by.group.and.type <-
    dcast(stops.merged[, .N, by = c('shift_id', 'contact_type')],
          shift_id ~ contact_type,
          value.var = 'N',
          fill = 0)

## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.by.group.and.type <- stops.by.group.and.type[, -'NA']

## rename for merge
colnames(stops.by.group.and.type)[
    -match(c('shift_id'),
           colnames(stops.by.group.and.type))] <- 'stops_n_' %.%
    colnames(stops.by.group.and.type)[
        -match(c('shift_id'),
               colnames(stops.by.group.and.type))]

## counts by civilian race
stops.by.group.and.race <-
    dcast(stops.merged[, .N, by = c('shift_id', 'civilian_race_short')],
          shift_id ~ civilian_race_short,
          value.var = 'N',
          fill = 0)

## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.by.group.and.race <- stops.by.group.and.race[, -'NA']

## rename for merge
colnames(stops.by.group.and.race)[
    -match(c('shift_id'),
           colnames(stops.by.group.and.race))] <- 'stops_n_' %.%
    colnames(stops.by.group.and.race)[
        -match(c('shift_id'),
               colnames(stops.by.group.and.race))]

## merge stop counts
assignments <- stops.by.group[assignments, on = c('shift_id')]
assignments <- stops.by.group.and.type[assignments, on = c('shift_id')]
assignments <- stops.by.group.and.race[assignments, on = c('shift_id')]

## write out results
write_csv(stops.merged, here("0-clean-data", "output", "stops-assignments_ba.csv"))
write_csv(assignments, here("0-clean-data", "output", "stops-outcomes_ba.csv"))
