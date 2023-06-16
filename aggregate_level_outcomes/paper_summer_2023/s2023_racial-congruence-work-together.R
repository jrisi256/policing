library(here)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)

################################################################################
# Read in individual-level data
dir <- here("aggregate_level_outcomes", "paper_summer_2023")

# Read in shift assignments (independent variables)
shift_assignments <- read_csv(file.path(dir, "officers_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <- read_csv(file.path(dir, "outcomes_ba_max.csv"))

# Read in unit level variables
unit_level <-
    read_csv(file.path(dir, "unit_level.csv")) %>%
    mutate(unit = as.character(unit))

# Create shift-level variable: racial diversity of officers working the shift
racial_diversity_shift_beat <-
    shift_assignments %>%
    count(unit, beat_assigned, date, shift, officer_race) %>%
    group_by(unit, beat_assigned, date, shift) %>%
    mutate(prcnt = n / sum(n)) %>%
    pivot_wider(id_cols = c("unit", "beat_assigned", "shift", "date"),
                names_from = "officer_race",
                values_from = c("n", "prcnt"),
                values_fill = 0) %>%
    ungroup() %>%
    mutate(n_officer = n_officer_black + n_officer_white + n_officer_hisp)

# Aggregate racial diversity to the unit-month (currently at beat-day)
racial_diversity_unit_month <-
    racial_diversity_shift_beat %>%
    filter(unit != 21 & unit != 23 & unit != 13) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    filter(year != 2012 & year != 2016) %>%
    mutate(diverse_shift =
               if_else(n_officer_white >= 1 & n_officer_black >= 1, 1, 0)) %>%
    mutate(white_only =
               if_else(n_officer_white > 1 & n_officer_white == n_officer, 1, 0),
           black_majority =
               if_else(prcnt_officer_black >= 0.5 & n_officer_white >= 1, 1, 0)) %>%
    group_by(unit, month, year) %>%
    summarise(nr_diverse_shifts = sum(diverse_shift),
              nr_white_only = sum(white_only),
              nr_majority_black = sum(black_majority),
              total_nr_shifts = n()) %>%
    ungroup() %>%
    mutate(unit = as.character(unit),
           prcnt_diverse = nr_diverse_shifts / total_nr_shifts,
           prcnt_white_only = nr_white_only / total_nr_shifts,
           prcnt_majority_black = nr_majority_black / total_nr_shifts)

unit_level_new <-
    unit_level %>%
    full_join(racial_diversity_unit_month) %>%
    mutate(unit = as.character(unit))

prcnt_diverse <-
    ggplot(unit_level_new, aes(x = black_ratio, y = prcnt_diverse)) +
    geom_point(aes(color = unit)) +
    theme_bw()

prcnt_white_only <-
    ggplot(unit_level_new, aes(x = black_ratio, y = prcnt_white_only)) +
    geom_point(aes(color = unit)) +
    theme_bw()

prcnt_majority_black <-
    ggplot(unit_level_new, aes(x = black_ratio, y = prcnt_majority_black)) +
    geom_point(aes(color = unit)) +
    theme_bw()

ggsave(filename = "fig_percent-shifts-diverse.png", plot = prcnt_diverse, path = dir)
ggsave(filename = "fig_percent_shifts-white-only.png", plot = prcnt_white_only, path = dir)
ggsave(filename = "fig_percent_shifts-black-majority.png", plot = prcnt_majority_black, path = dir)
