library(here)
source(here("functions.R"))

############################# Read in shift assignments (independent variables)
shift_assignments <-
    my_read_csv(here("create-outcomes",
                     "input",
                     "officers_assignments_ba.csv")) %>%
    select(officer_id, month, unit, date, shift, weekday, months_from_start,
           months_from_start_sq, shift_id, officer_race, officer_gender,
           spanish, beat_assigned)

count <- shift_assignments %>% count(date, shift, unit, beat_assigned)
mult_shift <-
    shift_assignments %>%
    group_by(date, shift, unit, beat_assigned) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    count(date, shift, unit, beat_assigned, officer_race) %>%
    pivot_wider(names_from = officer_race, values_from = n, values_fill = 0) %>%
    mutate(nr_races =
               as.numeric(officer_white > 0) +
               as.numeric(officer_black > 0) +
               as.numeric(officer_hisp > 0)) %>%
    mutate(type = case_when(nr_races == 3 ~ "all",
                            officer_white > 0 & officer_black > 0 ~ "Black + White",
                            officer_white > 0 & officer_hisp > 0 ~ "Hisp + White",
                            officer_black > 0 & officer_hisp > 0 ~ "Black + Hisp",
                            officer_white > 0 & officer_black == 0 & officer_hisp == 0 ~ "White only",
                            officer_black > 0 & officer_white == 0 & officer_hisp == 0 ~ "Black only",
                            officer_hisp > 0 & officer_black == 0 & officer_white == 0 ~ "Hisp only"))

######################################## Read in outcomes (dependent variables)
outcomes <-
    my_read_csv(here("create-outcomes", "output", "outcomes_ba_max.csv"),
                injured = T) %>%
    select(shift_id, stops_n, arrests_n, force_n)

#### Create shift-level independent variables capturing the racial diversity of
########################################### officers working a particular shift
shift_counts <-
    shift_assignments %>%
    count(unit, date, shift, officer_race) %>%
    group_by(unit, date, shift) %>%
    mutate(prcnt = n / sum(n)) %>%
    pivot_wider(id_cols = c("unit", "shift", "date"),
                names_from = "officer_race",
                values_from = c("n", "prcnt"),
                values_fill = 0) %>%
    ungroup() %>%
    mutate(n_officer = n_officer_black + n_officer_white + n_officer_hisp)

full_data <-
    shift_counts %>%
    select(-n_officer_black, -n_officer_white, -n_officer_hisp) %>%
    full_join(shift_assignments) %>%
    full_join(outcomes)

########### Get counts for mdsu (month, day, shift, unit), shifts, and officers
observations_per_mdsu <-
    full_data %>%
    distinct(unit, month, shift, weekday, date) %>%
    count(unit, month, shift, weekday)

officers_per_mdsu <-
    full_data %>%
    count(unit, month, shift, weekday)

officers_per_shift <-
    full_data %>%
    count(unit, date, shift, weekday)

ggplot(officers_per_shift, aes(x = n)) +
    geom_histogram(bins = 30) +
    theme_bw() +
    labs(x = "Number of Officers",
         title = "Number of Officers Per Shift") +
    geom_vline(xintercept = median(officers_per_shift$n), color = "red") +
    geom_vline(xintercept = summary(officers_per_shift$n)[["1st Qu."]],
               color = "blue") +
    geom_vline(xintercept = summary(officers_per_shift$n)[["3rd Qu."]],
               color = "green")

############################################################ Demean stops
stops_demean <-
    full_data %>%
    mutate(dummy = 1,
           officer_gender = case_when(officer_gender == "MALE" ~ "officer_male",
                                      officer_gender == "FEMALE" ~ "officer_female"),
           spanish = case_when(spanish == T ~ "officer_spanish",
                               spanish == F ~ "officer_english")) %>%
    pivot_longer(cols = c("officer_race", "officer_gender", "spanish"),
                 names_to = "column",
                 values_to = "value") %>%
    select(-column) %>%
    pivot_wider(names_from = "value",
                values_from = dummy,
                values_fill = 0) %>%
    filter(!is.na(stops_n)) %>%
    group_by(shift, weekday, unit, month) %>%
    mutate(across(c(stops_n, officer_white, officer_black, officer_hisp,
                    officer_male, officer_female, officer_spanish,
                    officer_english, months_from_start, months_from_start_sq,
                    prcnt_officer_black, prcnt_officer_white,
                    prcnt_officer_hisp, n_officer),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(stops_n_demean = stops_n - stops_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean,
           officer_male_demean = officer_male - officer_male_mean,
           officer_female_demean = officer_female - officer_female_mean,
           officer_spanish_demean = officer_spanish - officer_spanish_mean,
           officer_english_demean = officer_english - officer_english_mean,
           officer_exp_demean = months_from_start - months_from_start_mean,
           officer_exp_demean_sq = months_from_start_sq - months_from_start_sq_mean,
           prcnt_officer_black_demean = prcnt_officer_black - prcnt_officer_black_mean,
           prcnt_officer_white_demean = prcnt_officer_white - prcnt_officer_white_mean,
           prcnt_officer_hisp_demean = prcnt_officer_hisp - prcnt_officer_hisp_mean,
           n_officer_demean = n_officer - n_officer_mean) %>%
    ungroup()

stops_ols <-
    lm(stops_n_demean * 100 ~ officer_black_demean + officer_hisp_demean +
           officer_female_demean + officer_spanish_demean + officer_exp_demean +
           officer_exp_demean_sq + prcnt_officer_black_demean + 
           prcnt_officer_hisp_demean + n_officer_demean,
       data = stops_demean)

white_stops <- stops_demean %>% filter(officer_white == 1)
hisp_stops <- stops_demean %>% filter(officer_hisp == 1)
black_stops <- stops_demean %>% filter(officer_black == 1)

white_stops_ols <-
    lm(stops_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean + prcnt_officer_hisp_demean +
           n_officer_demean,
       data = white_stops)

black_stops_ols <-
    lm(stops_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean + prcnt_officer_hisp_demean +
           n_officer_demean,
       data = black_stops)

hisp_stops_ols <-
    lm(stops_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean + prcnt_officer_hisp_demean +
           n_officer_demean,
       data = hisp_stops)

############################################################ Demean arrests
arrests_demean <-
    full_data %>%
    mutate(dummy = 1,
           officer_gender = case_when(officer_gender == "MALE" ~ "officer_male",
                                      officer_gender == "FEMALE" ~ "officer_female"),
           spanish = case_when(spanish == T ~ "officer_spanish",
                               spanish == F ~ "officer_english")) %>%
    pivot_longer(cols = c("officer_race", "officer_gender", "spanish"),
                 names_to = "column",
                 values_to = "value") %>%
    select(-column) %>%
    pivot_wider(names_from = "value",
                values_from = dummy,
                values_fill = 0) %>%
    filter(!is.na(arrests_n)) %>%
    group_by(shift, weekday, unit, month) %>%
    mutate(across(c(arrests_n, officer_white, officer_black, officer_hisp,
                    officer_male, officer_female, officer_spanish,
                    officer_english, months_from_start, months_from_start_sq,
                    prcnt_officer_black, prcnt_officer_white,
                    prcnt_officer_hisp, n_officer),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(arrests_n_demean = arrests_n - arrests_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean,
           officer_male_demean = officer_male - officer_male_mean,
           officer_female_demean = officer_female - officer_female_mean,
           officer_spanish_demean = officer_spanish - officer_spanish_mean,
           officer_english_demean = officer_english - officer_english_mean,
           officer_exp_demean = months_from_start - months_from_start_mean,
           officer_exp_demean_sq = months_from_start_sq - months_from_start_sq_mean,
           prcnt_officer_black_demean = prcnt_officer_black - prcnt_officer_black_mean,
           prcnt_officer_white_demean = prcnt_officer_white - prcnt_officer_white_mean,
           prcnt_officer_hisp_demean = prcnt_officer_hisp - prcnt_officer_hisp_mean,
           n_officer_demean = n_officer - n_officer_mean) %>%
    ungroup()

arrests_ols <-
    lm(arrests_n_demean * 100 ~ officer_black_demean + officer_hisp_demean +
           officer_female_demean + officer_spanish_demean + officer_exp_demean +
           officer_exp_demean_sq + prcnt_officer_black_demean + 
           prcnt_officer_hisp_demean + n_officer_demean,
       data = arrests_demean)

white_arrests <- arrests_demean %>% filter(officer_white == 1)
hisp_arrests <- arrests_demean %>% filter(officer_hisp == 1)
black_arrests <- arrests_demean %>% filter(officer_black == 1)

white_arrests_ols <-
    lm(arrests_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = white_arrests)

black_arrests_ols <-
    lm(arrests_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = black_arrests)

hisp_arrests_ols <-
    lm(arrests_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = hisp_arrests)

############################################################ Demean force
force_demean <-
    full_data %>%
    mutate(dummy = 1,
           officer_gender = case_when(officer_gender == "MALE" ~ "officer_male",
                                      officer_gender == "FEMALE" ~ "officer_female"),
           spanish = case_when(spanish == T ~ "officer_spanish",
                               spanish == F ~ "officer_english")) %>%
    pivot_longer(cols = c("officer_race", "officer_gender", "spanish"),
                 names_to = "column",
                 values_to = "value") %>%
    select(-column) %>%
    pivot_wider(names_from = "value",
                values_from = dummy,
                values_fill = 0) %>%
    filter(!is.na(arrests_n)) %>%
    group_by(shift, weekday, unit, month) %>%
    mutate(across(c(force_n, officer_white, officer_black, officer_hisp,
                    officer_male, officer_female, officer_spanish,
                    officer_english, months_from_start, months_from_start_sq,
                    prcnt_officer_black, prcnt_officer_white,
                    prcnt_officer_hisp, n_officer),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(force_n_demean = force_n - force_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean,
           officer_male_demean = officer_male - officer_male_mean,
           officer_female_demean = officer_female - officer_female_mean,
           officer_spanish_demean = officer_spanish - officer_spanish_mean,
           officer_english_demean = officer_english - officer_english_mean,
           officer_exp_demean = months_from_start - months_from_start_mean,
           officer_exp_demean_sq = months_from_start_sq - months_from_start_sq_mean,
           prcnt_officer_black_demean = prcnt_officer_black - prcnt_officer_black_mean,
           prcnt_officer_white_demean = prcnt_officer_white - prcnt_officer_white_mean,
           prcnt_officer_hisp_demean = prcnt_officer_hisp - prcnt_officer_hisp_mean,
           n_officer_demean = n_officer - n_officer_mean) %>%
    ungroup()

force_ols <-
    lm(force_n_demean * 100 ~ officer_black_demean + officer_hisp_demean +
           officer_female_demean + officer_spanish_demean + officer_exp_demean +
           officer_exp_demean_sq + prcnt_officer_black_demean + 
           prcnt_officer_hisp_demean + n_officer_demean,
       data = force_demean)

white_force <- force_demean %>% filter(officer_white == 1)
hisp_force <- force_demean %>% filter(officer_hisp == 1)
black_force <- force_demean %>% filter(officer_black == 1)

white_force_ols <-
    lm(force_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = white_force)

black_force_ols <-
    lm(force_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = black_force)

hisp_force_ols <-
    lm(force_n_demean * 100 ~ officer_female_demean + officer_spanish_demean +
           officer_exp_demean + officer_exp_demean_sq +
           prcnt_officer_black_demean +  prcnt_officer_hisp_demean +
           n_officer_demean,
       data = hisp_force)

coefs <-
    tibble(coefs = c(coef(stops_ols)[["officer_black_demean"]],
                     coef(stops_ols)[["officer_hisp_demean"]],
                     coef(stops_ols)[["officer_female_demean"]],
                     coef(stops_ols)[["officer_spanish_demean"]],
                     coef(stops_ols)[["officer_exp_demean"]],
                     coef(stops_ols)[["prcnt_officer_black_demean"]],
                     coef(stops_ols)[["prcnt_officer_hisp_demean"]],
                     coef(stops_ols)[["n_officer_demean"]],
                     coef(arrests_ols)[["officer_black_demean"]],
                     coef(arrests_ols)[["officer_hisp_demean"]],
                     coef(arrests_ols)[["officer_female_demean"]],
                     coef(arrests_ols)[["officer_spanish_demean"]],
                     coef(arrests_ols)[["officer_exp_demean"]],
                     coef(arrests_ols)[["prcnt_officer_black_demean"]],
                     coef(arrests_ols)[["prcnt_officer_hisp_demean"]],
                     coef(arrests_ols)[["n_officer_demean"]],
                     coef(force_ols)[["officer_black_demean"]],
                     coef(force_ols)[["officer_hisp_demean"]],
                     coef(force_ols)[["officer_female_demean"]],
                     coef(force_ols)[["officer_spanish_demean"]],
                     coef(force_ols)[["officer_exp_demean"]],
                     coef(force_ols)[["prcnt_officer_black_demean"]],
                     coef(force_ols)[["prcnt_officer_hisp_demean"]],
                     coef(force_ols)[["n_officer_demean"]]),
           stderrors = c(1.96 * summary(stops_ols)$coefficients[,2][["officer_black_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["officer_hisp_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(stops_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["officer_black_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["officer_hisp_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(arrests_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["officer_black_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["officer_hisp_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(force_ols)$coefficients[,2][["n_officer_demean"]]),
           column = c(rep(c('Black', 'Hispanic', 'Female', 'Spanish-speaking',
                            'Experience', '% Black', '% Hispanic',
                            'Number of Officers'),
                          3)),
           outcome = c(rep("Stops", 8), rep("Arrests", 8), rep("Force", 8))) %>%
    mutate(column = fct_relevel(column,
                                'Number of Officers', '% Hispanic', '% Black',
                                'Experience', 'Spanish-speaking', 'Female',
                                'Hispanic', 'Black'),
           outcome = fct_relevel(outcome, "Stops", "Arrests", "Force"))

ggplot(coefs, aes(x = coefs, y = column)) +
    geom_point() +
    geom_errorbar(aes(xmin = coefs - stderrors,
                      xmax = coefs + stderrors),
                  width = 0.1) +
    facet_wrap(~outcome, scales = "free") +
    geom_vline(xintercept = 0) +
    theme_bw() +
    labs(x = "Coefficients",
         y = "Variables",
         title = "OLS Regression Coefficient Plot") +
    theme(text = element_text(size = 15))

race_coefs <-
    tibble(coefs = c(coef(white_stops_ols)[["officer_female_demean"]],
                     coef(white_stops_ols)[["officer_spanish_demean"]],
                     coef(white_stops_ols)[["officer_exp_demean"]],
                     coef(white_stops_ols)[["prcnt_officer_black_demean"]],
                     coef(white_stops_ols)[["prcnt_officer_hisp_demean"]],
                     coef(white_stops_ols)[["n_officer_demean"]],
                     coef(black_stops_ols)[["officer_female_demean"]],
                     coef(black_stops_ols)[["officer_spanish_demean"]],
                     coef(black_stops_ols)[["officer_exp_demean"]],
                     coef(black_stops_ols)[["prcnt_officer_black_demean"]],
                     coef(black_stops_ols)[["prcnt_officer_hisp_demean"]],
                     coef(black_stops_ols)[["n_officer_demean"]],
                     coef(hisp_stops_ols)[["officer_female_demean"]],
                     coef(hisp_stops_ols)[["officer_spanish_demean"]],
                     coef(hisp_stops_ols)[["officer_exp_demean"]],
                     coef(hisp_stops_ols)[["prcnt_officer_black_demean"]],
                     coef(hisp_stops_ols)[["prcnt_officer_hisp_demean"]],
                     coef(hisp_stops_ols)[["n_officer_demean"]],
                     coef(white_arrests_ols)[["officer_female_demean"]],
                     coef(white_arrests_ols)[["officer_spanish_demean"]],
                     coef(white_arrests_ols)[["officer_exp_demean"]],
                     coef(white_arrests_ols)[["prcnt_officer_black_demean"]],
                     coef(white_arrests_ols)[["prcnt_officer_hisp_demean"]],
                     coef(white_arrests_ols)[["n_officer_demean"]],
                     coef(black_arrests_ols)[["officer_female_demean"]],
                     coef(black_arrests_ols)[["officer_spanish_demean"]],
                     coef(black_arrests_ols)[["officer_exp_demean"]],
                     coef(black_arrests_ols)[["prcnt_officer_black_demean"]],
                     coef(black_arrests_ols)[["prcnt_officer_hisp_demean"]],
                     coef(black_arrests_ols)[["n_officer_demean"]],
                     coef(hisp_arrests_ols)[["officer_female_demean"]],
                     coef(hisp_arrests_ols)[["officer_spanish_demean"]],
                     coef(hisp_arrests_ols)[["officer_exp_demean"]],
                     coef(hisp_arrests_ols)[["prcnt_officer_black_demean"]],
                     coef(hisp_arrests_ols)[["prcnt_officer_hisp_demean"]],
                     coef(hisp_arrests_ols)[["n_officer_demean"]],
                     coef(white_force_ols)[["officer_female_demean"]],
                     coef(white_force_ols)[["officer_spanish_demean"]],
                     coef(white_force_ols)[["officer_exp_demean"]],
                     coef(white_force_ols)[["prcnt_officer_black_demean"]],
                     coef(white_force_ols)[["prcnt_officer_hisp_demean"]],
                     coef(white_force_ols)[["n_officer_demean"]],
                     coef(black_force_ols)[["officer_female_demean"]],
                     coef(black_force_ols)[["officer_spanish_demean"]],
                     coef(black_force_ols)[["officer_exp_demean"]],
                     coef(black_force_ols)[["prcnt_officer_black_demean"]],
                     coef(black_force_ols)[["prcnt_officer_hisp_demean"]],
                     coef(black_force_ols)[["n_officer_demean"]],
                     coef(hisp_force_ols)[["officer_female_demean"]],
                     coef(hisp_force_ols)[["officer_spanish_demean"]],
                     coef(hisp_force_ols)[["officer_exp_demean"]],
                     coef(hisp_force_ols)[["prcnt_officer_black_demean"]],
                     coef(hisp_force_ols)[["prcnt_officer_hisp_demean"]],
                     coef(hisp_force_ols)[["n_officer_demean"]]),
           stderrors = c(1.96 * summary(white_stops_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(white_stops_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(white_stops_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(white_stops_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(white_stops_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(white_stops_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(black_stops_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(hisp_stops_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(white_arrests_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(black_arrests_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(hisp_arrests_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(white_force_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(black_force_ols)$coefficients[,2][["n_officer_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["officer_female_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["officer_spanish_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["officer_exp_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["prcnt_officer_black_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["prcnt_officer_hisp_demean"]],
                         1.96 * summary(hisp_force_ols)$coefficients[,2][["n_officer_demean"]]),
           column = c(rep(c('Female', 'Spanish-speaking', 'Experience',
                            '% Black', '% Hispanic', 'Number of Officers'),
                          9)),
           outcome = c(rep("Stops", 18), rep("Arrests", 18), rep("Force", 18)),
           race = rep(rep(c("White", "Black", "Hispanic"), each = 6), 3)) %>%
    mutate(column = fct_relevel(column,
                                'Number of Officers', '% Hispanic', '% Black',
                                'Experience', 'Spanish-speaking', 'Female'),
           outcome = fct_relevel(outcome, "Stops", "Arrests", "Force"),
           race = fct_relevel(race, "White", "Black", "Hispanic"))

ggplot(race_coefs, aes(x = coefs, y = column)) +
    geom_point(aes(color = race), position = position_dodge(width = 1)) +
    geom_errorbar(aes(xmin = coefs - stderrors,
                      xmax = coefs + stderrors,
                      color = race),
                  width = 0.1,
                  position = position_dodge(width = 1)) +
    facet_wrap(~outcome, scales = "free") +
    geom_vline(xintercept = 0) +
    theme_bw()  +
    labs(x = "Coefficients",
         y = "Variables",
         title = "OLS Regression Coefficient Plot by Race") +
    theme(text = element_text(size = 16))
