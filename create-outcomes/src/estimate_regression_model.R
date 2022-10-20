library(here)
source(here("functions.R"))

outcomes <-
    my_read_csv(here("create-outcomes",
                     "output",
                     "outcomes_feasible_ba_max.csv")) %>%
    mutate(stops_n = stops_n * 100,
           arrests_n = arrests_n * 100,
           force_n = force_n * 100,
           officer_race = fct_relevel(officer_race,
                                      "officer_white",
                                      "officer_black",
                                      "officer_hisp"),
           officer_gender = fct_relevel(officer_gender, "MALE", "FEMALE"),
           month = as.factor(month)) %>%
    select(stops_n, arrests_n, force_n, month, shift, weekday, unit,
           officer_race, officer_gender, beat_assigned, stops_n_mean,
           arrests_n_mean, force_n_mean) %>%
    mutate(demean_stops = stops_n - stops_n_mean,
           demean_arrests = arrests_n - arrests_n_mean,
           demean_force = force_n - force_n_mean)

stops_race <- lm(demean_stops ~ officer_race, data = outcomes)
arrests_race <- lm(demean_arrests ~ officer_race, data = outcomes)
force_race <- lm(demean_force ~ officer_race, data = outcomes)

stops_race <- lm(stops_n ~ officer_race + month + shift + weekday + unit,
                 data = outcomes)

arrests_race <- lm(arrests_n ~ officer_race + month + shift + weekday + unit,
                   data = outcomes)
 
force_race <- lm(force_n ~ officer_race + month + shift + weekday + unit,
                 data = outcomes)

stops_sex <- lm(stops_n ~ officer_gender + month + shift + weekday + unit,
                 data = outcomes)

arrests_sex <- lm(arrests_n ~ officer_gender + month + shift + weekday + unit,
                   data = outcomes)

force_sex <- lm(force_n ~ officer_gender + month + shift + weekday + unit,
                 data = outcomes)
