library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(data.table)

# Read in data
assignments <- read_csv(here("bocar_data", "assignments_with_outcomes.csv"))

# Choose columns we wants (in our case only doing stops right now)
d <- 'officer_race'
ys <- colnames(assignments)[
    grep('^(stops|arrests|force)_n', colnames(assignments))
]
ys <- ys[-grep('civilian_(native|aapi|other|unknownrace)', ys)]

ys <- ys[str_which(ys, "stops")]

# Summarise results
assignmentsSummary <-
    assignments %>%
    group_by(officer_race) %>%
    summarise(across(ys, list(mean = mean, sd = sd), na.rm = T)) %>%
    ungroup()

assignmentsSummaryTotal <-
    assignments %>%
    summarise(across(ys, list(mean = mean, sd = sd), na.rm = T)) %>%
    mutate(officer_race = "total")

assignmentsSummary <- bind_rows(assignmentsSummary, assignmentsSummaryTotal)
