library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(fixest)
library(modelsummary)

################################################################################
dir <- here("aggregate_level_outcomes", "paper_summer_2023")

# Read in data
unit_level <- read_csv(file.path(dir, "unit_level.csv"))

unit_level <-
    unit_level %>%
    mutate(prcnt_officer_black = prcnt_officer_black * 100,
           property_cr_capita = property_cr * 10000 / total_pop,
           violent_cr_capita = violent_cr * 10000 /  total_pop,
           white_stop_rate = round(white_stops / white * 10000),
           hispanic_stop_rate = round(hispanic_stops / black * 10000),
           mean_years_worked_unit = mean_months_worked_unit / 12,
           year = as.character(year),
           unit = as.character(unit))

################################################################################
unit_level_model_vars <-
    unit_level %>%
    select(year, month, unit, black_stops, prcnt_officer_black, black_ratio,
           violent_cr_capita, property_cr_capita, log_total_officers, black,
           mean_years_worked_unit, black_arrests, black_force, nr_officer,
           black_stop_rate) %>%
    group_by(unit) %>%
    arrange(unit, year, month) %>%
    mutate(property_cr_capita_lag_1m = lag(property_cr_capita),
           violent_cr_capita_lag_1m = lag(violent_cr_capita),
           black_ratio_lag_1m = lag(black_ratio),
           prcnt_officer_black_lag_1m = lag(prcnt_officer_black),
           mean_years_worked_unit_lag_1m = lag(mean_years_worked_unit)) %>%
    ungroup()

################################################################################
nb_prcnt_exp <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_prcnt_exp_month <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year + month,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_prcnt_exp_full_lag <-
    femlm(
        black_stops ~
            prcnt_officer_black_lag_1m +
            mean_years_worked_unit +
            violent_cr_capita_lag_1m +
            property_cr_capita_lag_1m +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

################################################################################
# Models for black congruence and black stops with/without 1 month lags
nb_ratio_exp <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp_month <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year + month,
        cluster = c("unit"),
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp_full_lag <-
    femlm(
        black_stops ~
            black_ratio_lag_1m +
            mean_years_worked_unit +
            violent_cr_capita_lag_1m +
            property_cr_capita_lag_1m +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

##################################################### Display results in a table
# Table ???
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 3 from Table 2", "Yes"),
        `Model 2` = c("Model 3 (1 month lags)", "Yes"),
        `Model 3` = c("Model 4 from Table 2", "Yes"),
        `Model 4` = c("Model 4 (1 month lags)", "Yes")
    )
attr(offset_row, "position") <- c(1, 13)
rename =
    c(black_ratio = "Black Racial Congruence",
      black_ratio_lag_1m = "Black Racial Congruence (1 month lag)",
      prcnt_officer_black = "Percentage of Officers Who Are Black",
      prcnt_officer_black_lag_1m = "Percentage of Officers Who Are Black (1 month lag)",
      mean_years_worked_unit = "Years Worked In Unit (Mean)",
      mean_years_worked_unit_lag_1m = "Years Worked In Unit (Mean) (1 month lag)",
      property_cr_capita = "Property Crime Per 10,000",
      property_cr_capita_lag_1m = "Property Crime Per 10,000 (1 month lag)",
      violent_cr_capita = "Violent Crime Per 10,000",
      violent_cr_capita_lag_1m = "Violent Crime Per 10,000 (1 month lag)",
      log_total_officers = "Log of the Total Number of Officers",
      black_stops = "Number of stops of Black civilians"
    )

modelsummary(
    list(nb_ratio_exp,
         "Stops of Black Civilians" = nb_ratio_exp_full_lag,
         nb_prcnt_exp,
         nb_prcnt_exp_full_lag
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table_stops-aggregate-lag.txt"),
    add_rows = offset_row,
    notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
              "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
             )
    )

##################################################### Display results in a table
# Table ???
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 3 from Table 2", "Yes"),
        `Model 2` = c("Model 3 (Month)", "Yes"),
        `Model 3` = c("Model 4 from Table 2", "Yes"),
        `Model 4` = c("Model 4 (Month)", "Yes")
    )
attr(offset_row, "position") <- c(1, 9)
rename =
    c(black_ratio = "Black Racial Congruence",
      black_ratio_lag_1m = "Black Racial Congruence (1 month lag)",
      prcnt_officer_black = "Percentage of Officers Who Are Black",
      prcnt_officer_black_lag_1m = "Percentage of Officers Who Are Black (1 month lag)",
      mean_years_worked_unit = "Years Worked In Unit (Mean)",
      mean_years_worked_unit_lag_1m = "Years Worked In Unit (Mean) (1 month lag)",
      property_cr_capita = "Property Crime Per 10,000",
      property_cr_capita_lag_1m = "Property Crime Per 10,000 (1 month lag)",
      violent_cr_capita = "Violent Crime Per 10,000",
      violent_cr_capita_lag_1m = "Violent Crime Per 10,000 (1 month lag)",
      log_total_officers = "Log of the Total Number of Officers",
      black_stops = "Number of stops of Black civilians"
    )

modelsummary(
    list(nb_ratio_exp,
         "Stops of Black Civilians" = nb_ratio_exp_month,
         nb_prcnt_exp,
         nb_prcnt_exp_month
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table_stops-aggregate-month.txt"),
    add_rows = offset_row,
    notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
              "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
)

################################################################################
# Estimate regression models where dependent variable is arrests
nb_prcnt_arrest <-
    femlm(black_arrests ~
              prcnt_officer_black +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_ratio_arrest <-
    femlm(black_arrests ~
              black_ratio +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_prcnt_arrest_stopsiv <-
    femlm(black_arrests ~
              prcnt_officer_black +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              black_stops +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_ratio_arrest_stopsiv <-
    femlm(black_arrests ~
              black_ratio +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              black_stops +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

################################################################################
# Table ???
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model E", "Yes"),
        `Model 2` = c("Model F", "Yes"),
        `Model 3` = c("Model G", "Yes"),
        `Model 4` = c("Model H", "Yes")
    )
attr(offset_row, "position") <- c(1, 9)
f <- function(x) format(x, digits = 3, nsmall = 1, scientific = F)

modelsummary(
    list(nb_prcnt_arrest,
         "Arrests of Black Civilians" = nb_ratio_arrest,
         nb_prcnt_arrest_stopsiv,
         nb_ratio_arrest_stopsiv
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    fmt = f,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table_arrests-lag.txt"),
    add_rows = offset_row,
    notes =
        c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
          "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
         )
    )

################################################################################
# Estimate regression models where dependent variable is uses of force
nb_prcnt_force <-
    femlm(black_force ~
              prcnt_officer_black +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_ratio_force <-
    femlm(black_force ~
              black_ratio +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_prcnt_force_stopsiv <-
    femlm(black_force ~
              prcnt_officer_black +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              black_stops +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

nb_ratio_force_stopsiv <-
    femlm(black_force ~
              black_ratio +
              mean_years_worked_unit +
              violent_cr_capita_lag_1m +
              property_cr_capita_lag_1m +
              log_total_officers +
              black_stops +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level_model_vars)

################################################################################
# Table ???
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model I", "Yes"),
        `Model 2` = c("Model J", "Yes"),
        `Model 3` = c("Model K", "Yes"),
        `Model 4` = c("Model L", "Yes")
    )
attr(offset_row, "position") <- c(1, 9)

modelsummary(
    list(nb_prcnt_force,
         "Uses of force against Black civilians" = nb_ratio_force,
         nb_prcnt_force_stopsiv,
         nb_ratio_force_stopsiv),
    fmt = f,
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table_force-lag.txt"),
    add_rows = offset_row,
    notes =
        c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
          "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
         )
    )

################################################################################
# Property crime?
stops_property_cr <-
    femlm(black_stops ~ property_cr_capita + offset(log(black)),
          data = unit_level_model_vars)

stops_property_cr_fe <-
    femlm(black_stops ~ property_cr_capita + offset(log(black)) | unit,
          data = unit_level_model_vars)

arrests_property_cr <-
    femlm(black_arrests ~ property_cr_capita + offset(log(black)),
          data = unit_level_model_vars)

arrests_property_cr_fe <-
    femlm(black_arrests ~ property_cr_capita + offset(log(black)) | unit,
          data = unit_level_model_vars)

force_property_cr <-
    femlm(black_force ~ property_cr_capita + offset(log(black)),
          data = unit_level_model_vars)

force_property_cr_fe <-
    femlm(black_force ~ property_cr_capita + offset(log(black)) | unit,
          data = unit_level_model_vars)

offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Stops - Black Civilians", "Yes"),
        `Model 2` = c("Stops - Black Civilians", "Yes"),
        `Model 3` = c("Arrests - Black Civilians", "Yes"),
        `Model 4` = c("Arrests - Black Civilians", "Yes"),
        `Model 5` = c("Uses of Force - Black civilians", "Yes"),
        `Model 6` = c("Uses of Force - Black civilians", "Yes")
    )
attr(offset_row, "position") <- c(1, 6)

modelsummary(
    list(stops_property_cr,
         stops_property_cr_fe,
         arrests_property_cr,
         arrests_property_cr_fe,
         force_property_cr,
         force_property_cr_fe),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE|NA",
    exponentiate = T,
    output = file.path(dir, "table_property-crime.txt"),
    add_rows = offset_row,
    notes =
        c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
          "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)

################################################################################
# Property crime lagged?
stops_property_cr_lag <-
    femlm(black_stops ~ property_cr_capita_lag_1m + offset(log(black)),
          data = unit_level_model_vars)

stops_property_cr_fe_lag <-
    femlm(black_stops ~ property_cr_capita_lag_1m + offset(log(black)) | unit,
          data = unit_level_model_vars)

arrests_property_cr_lag <-
    femlm(black_arrests ~ property_cr_capita_lag_1m + offset(log(black)),
          data = unit_level_model_vars)

arrests_property_cr_fe_lag <-
    femlm(black_arrests ~ property_cr_capita_lag_1m + offset(log(black)) | unit,
          data = unit_level_model_vars)

force_property_cr_lag <-
    femlm(black_force ~ property_cr_capita_lag_1m + offset(log(black)),
          data = unit_level_model_vars)

force_property_cr_fe_lag <-
    femlm(black_force ~ property_cr_capita_lag_1m + offset(log(black)) | unit,
          data = unit_level_model_vars)

modelsummary(
    list(stops_property_cr_lag,
         stops_property_cr_fe_lag,
         arrests_property_cr_lag,
         arrests_property_cr_fe_lag,
         force_property_cr_lag,
         force_property_cr_fe_lag),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table_property-crime-lag.txt"),
    add_rows = offset_row,
    notes =
        c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
          "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
