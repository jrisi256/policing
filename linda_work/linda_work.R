library(here)
library(dplyr)
library(readr)
library(dtplyr)
library(ggplot2)
library(lubridate)
library(data.table)

##################################################################
##                         Read in data                         ##
##################################################################
officer_assignments <-
    read_csv(
        here(
            "aggregate_level_outcomes",
            "paper_summer_2023",
            "officers_assignments_ba.csv"
        )
    )

stops <-
    read_csv(here("aggregate_level_outcomes", "paper_summer_2023", "stops.csv"))

arrests <-
    read_csv(here("aggregate_level_outcomes", "paper_summer_2023", "arrests.csv"))

stops_assignments <-
    read_csv(here("linda_work", "stops_officers_assignments_ba_max.csv"))

arrests_assignments <-
    read_csv(here("linda_work", "arrests_officers_assignments_ba_max.csv"))

densitychart <- function(df, col, xtitle, ytitle = "Density") {
    raw_values <- df[[col]]
    boxplot_coord <- max(density(raw_values, na.rm = T)$y) / 2
    
    ggplot(df, aes(x = .data[[col]])) +
        geom_boxplot(aes(y = -boxplot_coord), width = boxplot_coord) +
        geom_density() +
        labs(x = xtitle, y = ytitle) +
        theme_bw()
}

##################################################################
##                  Number of units worked for                  ##
##################################################################
nr_units_worked_for <-
    officer_assignments %>%
    count(officer_id, unit) %>%
    count(officer_id)

ggplot(nr_units_worked_for, aes(x = n)) +
    geom_bar() +
    theme_bw() +
    labs(x = "Number of units worked for", y = "Number of officers")

#################################################################
##      Describing how regularly an officer works a shift      ##
#################################################################
shift_ids <-
    officer_assignments %>%
    group_by(unit, shift, beat_assigned) %>%
    mutate(shift_id = cur_group_id()) %>%
    ungroup()

nr_times_shift_worked_per_officer <-
    shift_ids %>%
    count(officer_id, unit, shift_id)

nr_times_working_shift <-
    shift_ids %>%
    filter(!is.na(start_datetime)) %>%
    arrange(officer_id, start_datetime, end_datetime) %>%
    select(
        officer_id,
        start_datetime,
        end_datetime,
        unit,
        shift,
        beat_assigned,
        shift_id
    ) %>%
    group_by(officer_id) %>%
    mutate(run_id = rleid(shift_id)) %>%
    group_by(officer_id, run_id) %>%
    mutate(n = n()) %>%
    ungroup()
    
nr_days_working_shift <-
    nr_times_working_shift %>%
    lazy_dt() %>%
    group_by(officer_id, run_id) %>%
    summarise(time = max(end_datetime) - min(start_datetime)) %>%
    mutate(time2 = day(seconds_to_period(time))) %>%
    ungroup() %>%
    as_tibble()

max_nr_shifts_per_officer <-
    nr_times_shift_worked_per_officer %>%
    group_by(officer_id, unit) %>%
    mutate(prcnt = n / sum(n) * 100) %>%
    filter(prcnt == max(prcnt)) %>%
    ungroup()

nr_unique_shifts_per_officer <-
    nr_times_shift_worked_per_officer %>%
    count(officer_id, unit)

densitychart(
    max_nr_shifts_per_officer,
    "prcnt",
    "The percentage of time an officer works their most common shift (per unit)"
)

densitychart(
    nr_unique_shifts_per_officer,
    "n",
    "The number of unique shifts worked by an officer (per unit)"
)

##################################################################
##         How many shifts does an officer work a week?         ##
##################################################################
nr_shifts_per_week <-
    officer_assignments %>%
    filter(!is.na(start_datetime)) %>%
    mutate(
        week_id = paste0(week(start_datetime), "_", year(start_datetime))
    ) %>%
    count(officer_id, week_id)

ggplot(nr_shifts_per_week, (aes(x = n))) +
    geom_bar() +
    theme_bw() +
    labs(
        x = "Number of shifts worked in one week",
        y = "Number of officer-week observations"
    )

ggplot(nr_shifts_per_week, (aes(x = n))) +
    geom_bar(aes(y = after_stat(count / sum(count)))) +
    theme_bw() +
    labs(
        x = "Number of shifts worked in one week",
        y = "Percentage of officer-week observations"
    )

nr_shifts_per_week_per_officer <-
    nr_shifts_per_week %>%
    group_by(officer_id) %>%
    summarise(
        avg_nr_shifts_per_week = mean(n),
        sd_nr_shifts_per_week = sd(n),
        total_nr_shifts = n()
    )
   
densitychart(
    nr_shifts_per_week_per_officer,
    "avg_nr_shifts_per_week",
    "The average number of shifts per week an officer works",
    "Number of officers (Density)"
)

densitychart(
    nr_shifts_per_week_per_officer,
    "total_nr_shifts",
    "Total number of shifts worked by an officer",
    "Number of officers (Density)"
)

#################################################################
##          How accurate is an officer's shift data?           ##
#################################################################
nr_officers_working_shift <-
    shift_ids %>%
    filter(!is.na(start_datetime)) %>%
    mutate(start_date = as_date(start_datetime)) %>%
    count(shift_id, start_date)

ggplot(nr_officers_working_shift, aes(x = n)) +
    geom_bar() +
    theme_bw() +
    labs(
        x = "Number of officers working a given shift",
        y = "Number of shifts"
    )

ggplot(nr_officers_working_shift, aes(x = n)) +
    geom_bar(aes(y = after_stat(count / sum(count)))) +
    theme_bw() +
    labs(
        x = "Number of officers working a given shift",
        y = "Number of shifts"
    )

# Some funkiness
# Sometimes a stop will not be reported as the first stop (even if only one officer made the stop)
# Sometimes an officer will have overlapping shifts so then a stop gets recorded as happening in both shifts
test <- 
    stops_assignments %>%
    filter(!is.na(stop_id)) %>%
    filter(!is.na(start_datetime)) %>%
    select(
        officer_id, unit, shift, beat_assigned, start_datetime, end_datetime,
        stop_id
    ) %>%
    group_by(officer_id, stop_id) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    full_join(
        select(
            stops, officer_id, stop_id, time, district, po_first, contact_type,
            civ.race, civ.gender, civ.age, lat, lon
        ),
        by = c("officer_id", "stop_id")
    ) %>%
    group_by(unit, shift, beat_assigned) %>%
    mutate(shift_id = as.character(cur_group_id())) %>%
    ungroup() %>%
    mutate(shift_id = if_else(is.na(beat_assigned), "off_duty", shift_id)) %>%
    group_by(stop_id) %>%
    summarise(nr_officers = n(), nr_shifts = n_distinct(shift_id)) %>%
    ungroup()
# group_by(stop_id) %>%
# summarise(sum = sum(po_first))

ggplot(test, aes(x = nr_officers)) +
    geom_bar() +
    theme_bw() +
    labs(x = "Number of times X officers made a stop together") +
    labs(y = "Number of stops")

test2 <- test %>% filter(nr_officers > 1)

ggplot(test2, aes(x = nr_shifts)) +
    geom_bar() +
    theme_bw() +
    labs(x = "Number of shifts represented when two officers make a stop together") +
    labs(y = "Number of stops involving 2 officers")
