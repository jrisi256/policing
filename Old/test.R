library(here)
library(readr)
library(dplyr)
library(lubridate)
library(data.table)

monthly_panel_units_1965 <-
    read_csv(here("New", "monthly_panel_units_1965-2016.csv.gz"))

# 2002 is a Subset of 1965 but starting in 2002 we have rank, month, and YOB
monthly_panel_units_2002 <-
    read_csv(here("New", "monthly-panel_2002-2016.csv.gz"))

# link_UID: Unique ID number for v2 which should be used on CPDP. Maps v1 UIDs to v0 old_UIDs but ensures there is a 1 to 1 relationship between link_UID and UID
# old_UID: Unique ID number in v0 for officers. Starts at 1. Does not match to v1 and v2 UIDs.
# UID: Unique ID number in v1 and v2 for officers. Starts at 100,001. Does not match to v0 UIDs (old_UID).

# Not sure why we have 3 files for unit history. My best guess right now is that unit history of a combination of all unit assignments from 03 and 12 without errors.
# Not sure what the difference is between 03 and 12. Each has records the other does not.
# Thinking I should just use unit history because it is the full set of 03 and 12 minus errors (missings and dates which do not make sense).
unit_history <- read_csv(here("New", "unit-history.csv.gz"))
unit_history_2016_03 <- read_csv(here("New", "unit-history__2016-03.csv.gz"))
unit_history_2016_12 <- read_csv(here("New", "unit-history__2016-12.csv.gz"))

roster <- read_csv(here("New", "roster__2018-03.csv.gz"))

profile <-
    read_csv(here("New", "final-profiles.csv.gz")) %>%
    mutate(appointed_month = floor_date(appointed_date))

profile_match <-
    profile %>%
    select(UID, link_UID, birth_year, race, gender, appointed_month) %>%
    mutate(
        officer_race =
            case_when(
                race == "HISPANIC" ~ "officer_hisp",
                race == "BLACK" ~ "officer_black",
                race == "WHITE" ~ "officer_white",
                race == "NATIVE AMERICAN/ALASKAN NATIVE" ~ "officer_native",
                race == "ASIAN/PAFICIAN ISLANDER" ~ "officer_aapi"
            ),
        officer_gender = gender
    ) %>%
    right_join(unit_history, by = c("UID", "link_UID"), multiple = "all") %>%
    arrange(UID, unit_start_date) #%>%
    # group_by(UID, birth_year, officer_race, officer_gender, appointed_month) %>%
    # summarise(unit_trajectory = paste0(unit, collapse = "_")) %>%
    # ungroup()

a2 <-
    profile_match %>%
    filter(race == "WHITE", gender == "MALE", birth_year == "1951", appointed_month == "1973-03-01")

officers <- read_csv(here("New", "officers.csv.gz")) %>% add_count(birth_year, appointed_month, officer_race, officer_gender)
officers_count <- officers %>% count(birth_year, appointed_month, officer_race, officer_gender)
assignments <- read_csv(here("New", "assignments.csv.gz"))
officer_unit <- assignments %>% count(officer_id, unit) %>% add_count(officer_id)

officers_on_patrol <- unique(assignments$officer_id)

officers_on_patrol_unit_assignments <-
    monthly_panel_units_1965 %>%
    filter(officer_id %in% officers_on_patrol) %>%
    left_join(officers, by = "officer_id") %>%
    arrange(officer_id, month) %>%
    group_by(officer_id) %>%
    mutate(run_id = rleid(unit)) %>%
    ungroup() %>%
    distinct(officer_id, run_id, .keep_all = T) %>%
    group_by(officer_id, birth_year, appointed_month, officer_race, officer_gender) %>%
    summarise(unit_trajectory = paste0(unit, collapse = "_")) %>%
    ungroup()

a <- officers_on_patrol_unit_assignments %>% filter(officer_id == 21592)

match <-
    officers_on_patrol_unit_assignments %>%
    left_join(
        profile_match,
        by = c("officer_race", "officer_gender", "birth_year", "appointed_month", "unit_trajectory")
    )

a3 <- match %>% group_by(officer_id) %>% filter(n() > 1)

a4 <-
    profile_match %>%
    filter(UID %in% c(105056, 120166, 124714)) %>%
    arrange(UID, unit_start_date)

a5 <- a3 %>% filter(UID %in% c(105056, 120166, 124714))

a6 <- monthly_panel_units_1965 %>% filter(officer_id %in% a5$officer_id)

a7 <- profile %>% filter(UID %in% a5$UID)

a8 <- officers %>% filter(officer_id %in% a5$officer_id)

# Tried to see if there is a pattern in IDs, and I could not find one.
# This analysis is mostly trash.
officers_on_patrol_unique <-
    officers %>%
    filter(officer_id %in% officers_on_patrol) %>%
    left_join(officers_count) %>%
    filter(n == 1)

compare <-
    officers_on_patrol_unique %>%
    left_join(profile_unique)

