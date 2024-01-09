library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

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
nr_times_shift_worked_per_officer <-
    officer_assignments %>%
    group_by(unit, shift, beat_assigned) %>%
    mutate(shift_id = cur_group_id()) %>%
    ungroup() %>%
    count(officer_id, unit, shift_id)

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
##          How often does an officer's shift change?          ##
#################################################################
test <-
    officer_assignments %>%
    group_by(unit, shift, beat_assigned) %>%
    mutate(shift_id = cur_group_id()) %>%
    ungroup() %>%
    arrange(officer_id, start_datetime) %>%
    group_by(officer_id) %>%
    reframe(rle = rle(shift_id)$lengths,
            shift_ids = rle(shift_id)$values)

