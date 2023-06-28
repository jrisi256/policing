library(here)
library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(lubridate)
library(modelsummary)
library(ggplot2)

################################################################################
# Read in individual-level data
dir <- here("aggregate_level_outcomes", "paper_summer_2023")

# Read in shift assignments (independent variables)
shift_assignments <- read_csv(file.path(dir, "officers_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <- read_csv(file.path(dir, "outcomes_ba_max.csv"))

# Read in level 2 variables (police unit)
unit_level <-
    read_csv(file.path(dir, "unit_level.csv")) %>%
    mutate(prcnt_officer_black = prcnt_officer_black * 100,
           property_cr_capita = property_cr * 10000 / total_pop,
           violent_cr_capita = violent_cr * 10000 /  total_pop,
           white_stop_rate = round(white_stops / white * 10000),
           hispanic_stop_rate = round(hispanic_stops / black * 10000),
           mean_years_worked_unit = mean_months_worked_unit / 12,
           year = as.character(year),
           unit = as.character(unit)) %>%
    mutate(prcnt_poverty_scale = scale(prcnt_poverty)[, 1],
           prcnt_notlf_scale = scale(prcnt_notlf)[, 1],
           prcnt_single_scale = scale(prcnt_single)[, 1],
           prcnt_civ_black_scale = scale(prcnt_civ_black)[, 1],
           disadvantage_new = (prcnt_poverty_scale + prcnt_notlf_scale + prcnt_single_scale) / 3,
           disadvantage_black_new = (prcnt_poverty_scale + prcnt_notlf_scale + prcnt_single_scale + prcnt_civ_black_scale) / 4) %>%
    select(unit, month, year, black_ratio, prcnt_officer_black, disadvantage,
           prcnt_civ_black, disadvantage_new, disadvantage_black_new, black) %>%
    rename(prcnt_officer_black_unit = prcnt_officer_black)

################################################################################
# Clean data

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

# Merge racial diversity of shifts with other independent variables
full_data_beats <-
    racial_diversity_shift_beat %>%
    full_join(shift_assignments, multiple = "all") %>%
    full_join(outcomes)

# Create dummy variables
full_data_beats <-
    full_data_beats %>%
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
                values_fill = 0)

# Filter out observations missing experience, create shift diversity variables
full_data_beats <-
    full_data_beats %>%
    filter(!is.na(months_from_start)) %>%
    mutate(mult_officer = if_else(n_officer > 1, 1, 0),
           n_officer_black = if_else(officer_black == 1, n_officer_black - 1, n_officer_black),
           n_officer_hisp = if_else(officer_hisp == 1, n_officer_hisp - 1, n_officer_hisp),
           n_officer_white = if_else(officer_white == 1, n_officer_white - 1, n_officer_white)) %>%
    mutate(unit = as.character(unit)) %>%
    rename(`Police Unit` = unit,
           `Month-Year` = month,
           `Individual Officer` = officer_id) %>%
    mutate(years_exp = months_from_start / 12,
           years_exp_sq = years_exp ^ 2,
           month = month(date),
           year = as.character(year(date)))

################################################################################
# Add in level 2 variables
full_data_beats_lvl2 <-
    full_data_beats %>%
    inner_join(unit_level, by = c("Police Unit" = "unit", "month", "year"))

stops_df_lvl2 <- full_data_beats_lvl2 %>% filter(!is.na(stops_n))
arrests_df_lvl2 <- full_data_beats_lvl2 %>% filter(!is.na(arrests_n))
force_df_lvl2 <- full_data_beats_lvl2 %>% filter(!is.na(force_n))

# There are only 68 mdsbs with different values at level 2.
# Within them, there are only a few shifts being compared compared.
nr_mdsb_comparisons <-
    stops_df_lvl2 %>%
    group_by(beat_assigned, `Month-Year`, weekday, shift) %>%
    filter(n_distinct(prcnt_officer_black_unit) > 1) %>%
    filter(!(all(stops_black == 0))) %>%
    mutate(id =  cur_group_id()) %>%
    ungroup() %>%
    select(stops_black, prcnt_officer_black_unit, beat_assigned, `Month-Year`,
           shift, weekday, `Police Unit`, id) %>%
    arrange(`Month-Year`, shift, beat_assigned, weekday)

ggplot(nr_mdsb_comparisons,
       aes(x = prcnt_officer_black_unit, y = stops_black)) +
    geom_point() +
    facet_wrap(~id) +
    geom_smooth(method = "lm")

# All of these regression models are trash.
################################################################################
# estimate number of stops at the individual-shift level
individual_stops_lvl2_ratio <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_hisp +
            black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df_lvl2)

individual_stops_lvl2_prcnt <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df_lvl2)

individual_stops_lvl2_ratio_interaction <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            black_ratio +
            officer_black * black_ratio +
            officer_hisp * black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df_lvl2)

individual_stops_lvl2_prcnt_interaction <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit +
            officer_black * prcnt_officer_black_unit +
            officer_hisp * prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df_lvl2)

################################################################################
# estimate number of arrests at the individual-shift level
individual_arrests_lvl2_ratio <-
    fepois(
        arrests_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = arrests_df_lvl2)

individual_arrests_lvl2_prcnt <-
    fepois(
        arrests_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = arrests_df_lvl2)

individual_arrests_lvl2_ratio_interaction <-
    fepois(
        arrests_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            black_ratio +
            officer_black * black_ratio +
            officer_hisp * black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = arrests_df_lvl2)

individual_arrests_lvl2_prcnt_interaction <-
    fepois(
        arrests_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit +
            officer_black * prcnt_officer_black_unit +
            officer_hisp * prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = arrests_df_lvl2)

################################################################################
# estimate uses of force at the individual-shift level
individual_force_lvl2_ratio <-
    fepois(
        force_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = force_df_lvl2)

individual_force_lvl2_prcnt <-
    fepois(
        force_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = force_df_lvl2)

individual_force_lvl2_ratio_interaction <-
    fepois(
        force_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            black_ratio +
            officer_black * black_ratio +
            officer_hisp * black_ratio | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = force_df_lvl2)

individual_force_lvl2_prcnt_interaction <-
    fepois(
        force_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp +
            prcnt_officer_black_unit +
            officer_black * prcnt_officer_black_unit +
            officer_hisp * prcnt_officer_black_unit | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = force_df_lvl2)

################################################################################
# Create table for stops
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Negative Binomial Regression", "X", "X", "X", "X"),
           `Model 2` = c("Negative Binomial Regression", "X", "X", "X", "X"),
           `Model 3` = c("Negative Binomial Regression", "X", "X", "X", "X"),
           `Model 4` = c("Negative Binomial Regression", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 23, 24, 25, 26)

modelsummary(list(individual_stops_lvl2_ratio,
                  "Stops of Black civilians" = individual_stops_lvl2_prcnt,
                  individual_stops_lvl2_ratio_interaction,
                  individual_stops_lvl2_prcnt_interaction),
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift",
                             black_ratio = "Black Racial Congruence",
                             prcnt_officer_black_unit = "Percentage of unit that is Black"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-results_stops-lvl2.txt"),
             add_rows = offset_row,
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# Create table for arrests
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 2` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 3` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 4` = c("Poisson Regression", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 19, 20, 21, 22)

modelsummary(list(individual_arrests_lvl2_ratio,
                  "Arrests of Black civilians" = individual_arrests_lvl2_prcnt,
                  individual_arrests_lvl2_ratio_interaction,
                  individual_arrests_lvl2_prcnt_interaction),
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift",
                             black_ratio = "Black Racial Congruence",
                             prcnt_officer_black_unit = "Percentage of unit that is Black"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-results_arrests-lvl2.txt"),
             add_rows = offset_row,
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# Create table for force
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 2` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 3` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 4` = c("Poisson Regression", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 19, 20, 21, 22)

modelsummary(list(individual_force_lvl2_ratio,
                  "Uses of force against Black civilians" = individual_force_lvl2_prcnt,
                  individual_force_lvl2_ratio_interaction,
                  individual_force_lvl2_prcnt_interaction),
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift",
                             black_ratio = "Black Racial Congruence",
                             prcnt_officer_black_unit = "Percentage of unit that is Black"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-results_force-lvl2.txt"),
             add_rows = offset_row,
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))
