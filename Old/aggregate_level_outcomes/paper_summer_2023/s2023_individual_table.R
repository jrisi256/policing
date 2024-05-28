library(here)
library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(flextable)
library(modelsummary)

################################################################################
# Read in individual-level data
dir <- here("New", "aggregate_level_outcomes", "paper_summer_2023")

# Read in shift assignments (independent variables)
shift_assignments <- read_csv(file.path(dir, "officers_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <- read_csv(file.path(dir, "outcomes_ba_max.csv"))

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
           `Police Unit` = as.character(`Police Unit`))

stops_df <- full_data_beats %>% filter(!is.na(stops_n))
white_civ_stops <- full_data_beats %>% filter(!is.na(stops_white))
hisp_civ_stops <- full_data_beats %>% filter(!is.na(stops_hispanic))

white_stops_df <- stops_df %>% filter(officer_white == 1)
black_stops_df <- stops_df %>% filter(officer_black == 1)

arrests_df <- full_data_beats %>% filter(!is.na(arrests_n))
force_df <- full_data_beats %>% filter(!is.na(force_n))

#############################################################################
##  Reviewer comment from R&R #2, Estimate models for different civilians. ##
#############################################################################
individual_stops <-
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
            n_officer_white | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
            #scale(n_officer_black) +
            #scale(n_officer_white) +
            #scale(n_officer_hisp) | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df)

individual_stops_all <-
    fenegbin(
        stops_n ~
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
        data = stops_df
    )

individual_stops_white_civ <-
    fenegbin(
        stops_white ~
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
        data = white_civ_stops
    )

individual_stops_hisp_civ <-
    fenegbin(
        stops_hispanic ~
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
        data = hisp_civ_stops
    )

offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"),
           `Model 4` = c("", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(list("Stops of All Civilians" = individual_stops_all,
                  "Stops of Black Civilians" = individual_stops,
                  "Stops of White Civilians" = individual_stops_white_civ,
                  "Stops of Hispanic Civilians" = individual_stops_hisp_civ),
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
             fmt = 4,
             stars = T,
             output = file.path(dir, "civ-race_individual-results.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# estimate number of stops, arrests, and force at the individual-shift level
white_stops <-
    fenegbin(
        stops_black ~
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp| beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = white_stops_df)

black_stops <-
    fenegbin(
        stops_black ~
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = black_stops_df)

num <- (white_stops$coefficients[["n_officer_black"]] - black_stops$coefficients[["n_officer_black"]])
denom <- sqrt(white_stops$se[["n_officer_black"]] ^ 2 + black_stops$se[["n_officer_black"]] ^ 2)
test_coef <- 2 * pnorm(abs(num / denom), lower = F)

num <- (white_stops$coefficients[["n_officer_white"]] - black_stops$coefficients[["n_officer_white"]])
denom <- sqrt(white_stops$se[["n_officer_white"]] ^ 2 + black_stops$se[["n_officer_white"]] ^ 2)
test_coef <- 2 * pnorm(abs(num / denom), lower = F)

individual_stops_no_mult <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df)

################################################################################
# Reviewer comment - Estimate regression models for each racial group
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 14, 15, 16, 17)

modelsummary(list("All Officers" = individual_stops,
    "Black officers" = black_stops,
                  "White officers" = white_stops),
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
             output = file.path(dir, "all-black-white_individual-results.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             fmt = 4,
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# Reviewer comment no multiple officers variable
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Model from Table 4", "X", "X", "X", "X"),
           `Model 2` = c("Model from Table 4 (no multiple officers variable)", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(list("Stops of Black civilians" = individual_stops,
                  individual_stops_no_mult),
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
             output = file.path(dir, "tableReveiwer3-7_individual-results.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
individual_stops_no_exp_sq <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df)

individual_arrests <-
    fepois(arrests_black ~
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
           data = arrests_df)

individual_force <-
    fepois(force_black ~
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
           data = force_df)

################################################################################
# Reviewer comment no experience squared
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Model from Table 4", "X", "X", "X", "X"),
           `Model 2` = c("Model from Table 4 (no experience squared)", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(list("Stops of Black civilians" = individual_stops,
                  individual_stops_no_exp_sq),
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
             output = file.path(dir, "tableReveiwer3-6_individual-results.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# Create Table A4
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("Negative Binomial Regression", "X", "X", "X", "X"),
           `Model 2` = c("Poisson Regression", "X", "X", "X", "X"),
           `Model 3` = c("Poisson Regression", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(list("Stops of Black civilians" = individual_stops,
                  "Arrests of Black civilians" = individual_arrests,
                  "Uses of force against Black civilians" = individual_force),
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
             fmt = 3,
             stars = T,
             output = file.path(dir, "tableA4_individual-results.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

################################################################################
# stops, arrests, and force at the individual-shift level (binary outcome)
individual_stops_bin <-
    feglm(
        stops_black_bin ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        family = "logit",
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df)

individual_arrests_bin <-
    feglm(arrests_black_bin ~
              officer_black +
              officer_hisp +
              officer_female +
              years_exp +
              years_exp_sq +
              mult_officer +
              n_officer_black +
              n_officer_white +
              n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
          family = "logit",
          cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
          data = arrests_df)

individual_force_bin <-
    feglm(force_black_bin ~
              officer_black +
              officer_hisp +
              officer_female +
              years_exp +
              years_exp_sq +
              mult_officer +
              n_officer_black +
              n_officer_white +
              n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
          family = "logit",
          cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
          data = force_df)

################################################################################
# Recreate Table A4 using logistic regression instead of count-based models
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("Logistic Regression", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(list("Stops of Black civilians (1/0)" = individual_stops_bin,
                  "Arrests of Black civilians (1/0)" = individual_arrests_bin,
                  "Uses of force against Black civilians (1/0)" = individual_force_bin),
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
             output = file.path(dir, "tableA4_individual-results-logit.txt"),
             add_rows = offset_row,
             coef_omit = ".theta",
             gof_omit = "FE:|RMSE|AIC|R2 Within$",
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))
