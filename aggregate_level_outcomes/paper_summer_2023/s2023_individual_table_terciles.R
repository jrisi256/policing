library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fixest)
library(modelsummary)

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
           prcnt_civ_black, disadvantage_new, disadvantage_black_new, black,
           prcnt_poverty) %>%
    rename(prcnt_officer_black_unit = prcnt_officer_black) %>%
    distinct(unit, .keep_all = T) %>%
    mutate(officer_black_ntile = ntile(prcnt_officer_black_unit, 3),
           poverty_ntile = ntile(prcnt_poverty, 3)) %>%
    select(officer_black_ntile, poverty_ntile, unit) %>%
    pivot_longer(
        cols = matches("ntile"), names_to = "variable", values_to = "values") %>%
    unite("variable", c("variable", "values"))

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
    rename(`Police Unit` = unit,
           `Month-Year` = month,
           `Individual Officer` = officer_id) %>%
    mutate(years_exp = months_from_start / 12,
           years_exp_sq = years_exp ^ 2,
           stops_black_bin = if_else(stops_black == 0, 0, 1),
           arrests_black_bin = if_else(arrests_black == 0, 0, 1),
           force_black_bin = if_else(force_black == 0, 0, 1),
           `Police Unit` = as.character(`Police Unit`)) %>%
    select(stops_black, officer_black, officer_hisp, officer_female, years_exp,
           years_exp_sq, mult_officer, n_officer_black, n_officer_white,
           n_officer_hisp, beat_assigned, `Month-Year`, weekday, shift,
           `Police Unit`, arrests_black, force_black, `Individual Officer`)

################################################################################
# Split by percent of unit that is black and poverty
full_data_beats_tercile <-
    full_data_beats %>%
    inner_join(unit_level, by = c("Police Unit" = "unit"), multiple = "all") %>%
    group_split(variable)

names <- unlist(map(full_data_beats_tercile,
                    function(df) {df %>% pull(variable) %>% unique()}))

names[names == "poverty_ntile_1"] <- "Low poverty"
names[names == "poverty_ntile_2"] <- "Medium poverty"
names[names == "poverty_ntile_3"] <- "High poverty"
names[names == "officer_black_ntile_1"] <- "Low % of Black Officers"
names[names == "officer_black_ntile_2"] <- "Medium % of Black Officers"
names[names == "officer_black_ntile_3"] <- "High % of Black Officers"
names(full_data_beats_tercile) <- names

################################################################################
# Estimate model for stops
regression_group_stops <-
    map(full_data_beats_tercile,
        function(df) {
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
                    n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
                cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
                data = df)
        })

offset_row_stops <-
    tibble(term = c("Black Stops (Negative Binomial Regression)",
                    "FE - Day of the Week",
                    "FE - Month-Year",
                    "FE - Shift Timing",
                    "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"),
           `Model 4` = c("", "X", "X", "X", "X"),
           `Model 5` = c("", "X", "X", "X", "X"),
           `Model 6` = c("", "X", "X", "X", "X"))
attr(offset_row_stops, "position") <- c(1, 13, 14, 15, 16, 17, 18)

modelsummary(regression_group_stops,
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-stops-terciles.txt"),
             add_rows = offset_row_stops,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# From the R&R, estimate if the difference between coefficients is statistically
# significant across models

low_black_model <- regression_group_stops[["Low % of Black Officers"]]
low_black_officer_black_coef <- low_black_model$coefficients[["officer_black"]]
low_black_officer_black_se <- low_black_model$se[["officer_black"]]
low_black_nr_black_officer_coef <- low_black_model$coefficients[["n_officer_black"]]
low_black_nr_black_officer_se <- low_black_model$se[["n_officer_black"]]

med_black_model <- regression_group_stops[["Medium % of Black Officers"]]
med_black_officer_black_coef <- med_black_model$coefficients[["officer_black"]]
med_black_officer_black_se <- med_black_model$se[["officer_black"]]
med_black_nr_black_officer_coef <- med_black_model$coefficients[["n_officer_black"]]
med_black_nr_black_officer_se <- med_black_model$se[["n_officer_black"]]

high_black_model <- regression_group_stops[["High % of Black Officers"]]
high_black_officer_black_coef <- high_black_model$coefficients[["officer_black"]]
high_black_officer_black_se <- high_black_model$se[["officer_black"]]
high_black_nr_black_officer_coef <- high_black_model$coefficients[["n_officer_black"]]
high_black_nr_black_officer_se <- high_black_model$se[["n_officer_black"]]

compare_coefficients <- function(c1, c2, se1, se2) {
    z_statistic <- (c1 - c2) / sqrt(se1 ^ 2 + se2 ^ 2)
    p_value <- 2 * pnorm(abs(z_statistic), lower = F)
    return(list(z_statistic = z_statistic, p_value = p_value))
}

coefficient_comparison_test <-
    pmap(
        list(
            c1 = 
                list(
                    low_black_nr_black_officer_coef,
                    low_black_nr_black_officer_coef,
                    med_black_nr_black_officer_coef,
                    low_black_officer_black_coef,
                    low_black_officer_black_coef,
                    med_black_officer_black_coef
                ),
            c2 =
                list(
                    med_black_nr_black_officer_coef,
                    high_black_nr_black_officer_coef,
                    high_black_nr_black_officer_coef,
                    med_black_officer_black_coef,
                    high_black_officer_black_coef,
                    high_black_officer_black_coef
                ),
            se1 = 
                list(
                    low_black_nr_black_officer_se,
                    low_black_nr_black_officer_se,
                    med_black_nr_black_officer_se,
                    low_black_officer_black_se,
                    low_black_officer_black_se,
                    med_black_officer_black_se
                ),
            se2 = list(
                med_black_nr_black_officer_se,
                high_black_nr_black_officer_se,
                high_black_nr_black_officer_se,
                med_black_officer_black_se,
                high_black_officer_black_se,
                high_black_officer_black_se
            )
        ),
        compare_coefficients
    )

################################################################################
# Estimate model for arrests
regression_group_arrests <-
    map(full_data_beats_tercile,
        function(df) {
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
                    n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
                cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
                data = df)
        })

offset_row_arrests <-
    tibble(term = c("Black Arrests (Poisson Regression)",
                    "FE - Day of the Week",
                    "FE - Month-Year",
                    "FE - Shift Timing",
                    "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"),
           `Model 4` = c("", "X", "X", "X", "X"),
           `Model 5` = c("", "X", "X", "X", "X"),
           `Model 6` = c("", "X", "X", "X", "X"))
attr(offset_row_arrests, "position") <- c(1, 13, 14, 15, 16, 17, 18)

modelsummary(regression_group_arrests,
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-arrests-terciles.txt"),
             add_rows = offset_row_arrests,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# Estimate model for force
regression_group_force <-
    map(full_data_beats_tercile,
        function(df) {
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
                    n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
                cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
                data = df)
        })

offset_row_force <-
    tibble(term = c("Uses of Force - Black Civilians (Poisson Regression)",
                    "FE - Day of the Week",
                    "FE - Month-Year",
                    "FE - Shift Timing",
                    "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"),
           `Model 4` = c("", "X", "X", "X", "X"),
           `Model 5` = c("", "X", "X", "X", "X"),
           `Model 6` = c("", "X", "X", "X", "X"))
attr(offset_row_force, "position") <- c(1, 13, 14, 15, 16, 17, 18)

modelsummary(regression_group_force,
             coef_rename = c(officer_black = "Officer Race/Ethnicity - Black",
                             officer_hisp = "Officer Race/Ethniciy - Hispanic",
                             officer_female = "Officer Sex - Female",
                             years_exp = "Officer Experience (Years)",
                             years_exp_sq = "Officer Experience Squared (Years)",
                             mult_officer = "Multipe officers assigned to the shift?",
                             n_officer_black = "Number of other Black officers on shift",
                             n_officer_white = "Number of other White officers on shift",
                             n_officer_hisp = "Number of other Hispanic officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             exponentiate = T,
             statistic = NULL,
             stars = T,
             output = file.path(dir, "table_individual-force-terciles.txt"),
             add_rows = offset_row_force,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))
