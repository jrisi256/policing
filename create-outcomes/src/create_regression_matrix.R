library(here)
source(here("functions.R"))

# Read in shift assignments (independent variables)
shift_assignments <- my_read_csv(here("create-outcomes",
                                      "input",
                                      "officer_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <- my_read_csv(here("create-outcomes",
                             "output",
                             "outcomes_ba_max.csv"),
                        injured = T)

# Use most frequent home unit of assigned officer as the home unit for each beat
# Some beats cannot be assigned this way (occur in each unit same nr. of times)
# So for now, I just randomly pick them
find_beat_home <-
    shift_assignments %>%
    count(beat_assigned, unit) %>%
    group_by(beat_assigned) %>%
    filter(n == max(n)) %>%
    distinct(beat_assigned, .keep_all = T) %>%
    select(-n)

groups_by_date <-
    shift_assignments %>%
    count(beat_assigned, weekday, shift, month, date, officer_race) %>%
    pivot_wider(names_from = officer_race, values_from = n, values_fill = 0) %>%
    mutate(n = officer_white + officer_black + officer_hisp,
           bw_comparisons_samedate = officer_black * officer_white,
           hw_comparisons_samedate = officer_hisp * officer_white)

groups_by_mdsb <-
    groups_by_date %>%
    group_by(beat_assigned, weekday, shift, month) %>%
    summarise(across(officer_white:hw_comparisons_samedate, sum)) %>%
    mutate(nr_races =
               as.numeric(officer_white > 0) +
               as.numeric(officer_black > 0) +
               as.numeric(officer_hisp > 0),
           bw_comparisons = officer_white * officer_black,
           hw_comparisons = officer_white * officer_hisp) %>%
    full_join(find_beat_home, by = "beat_assigned")

feasible_groups <-
    groups_by_mdsb %>%
    filter(nr_races > 1) %>%
    select(beat_assigned, weekday, shift, month)

# keep only shift assignments with more than one racial group
# those with only one racial group will contribute 0 to the estimate
shift_assignments_feasible <-
    inner_join(shift_assignments, feasible_groups) %>%
    inner_join(outcomes, by = "shift_id") %>%
    mutate(officer_race = fct_relevel(officer_race, "officer_white", "officer_black", "officer_hisp"),
           month = as.factor(month))

demean <-
    shift_assignments_feasible %>%
    group_by(month, shift, weekday, beat_assigned) %>%
    summarise(across(c(stops_n, arrests_n, force_n), mean, na.rm = T)) %>%
    rename(stops_n_mean = stops_n,
           arrests_n_mean = arrests_n,
           force_n_mean = force_n)

shift_assignments_feasible <-
    shift_assignments_feasible %>%
    inner_join(demean, by = c("month", "weekday", "shift", "beat_assigned"))

write_csv(shift_assignments_feasible,
          here("create-outcomes", "output", "outcomes_feasible_ba_max.csv"))
