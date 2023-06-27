library(here)
library(readr)
library(dplyr)
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
           unit = as.character(unit),
           percent10 = prcnt_officer_black >= 10,
           percent15 = prcnt_officer_black >= 15,
           percent20 = prcnt_officer_black >= 20,
           percent25 = prcnt_officer_black >= 25,
           percent30 = prcnt_officer_black >= 30)

################################################################################
# Estimate regression models for different thresholds
nb_ratio <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level
    )

nb_ratio_t10 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent10 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_ratio_t15 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent15 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_ratio_t20 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent20 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_ratio_t25 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent25 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_ratio_t30 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent30 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

offset_row <- tibble(term = c("", "Offset - Black Pop."),
                     `Model 1` = c("No threshold", "Yes"),
                     `Model 2` = c("Threshold 10%", "Yes"),
                     `Model 3` = c("Threshold 15%", "Yes"),
                     `Model 4` = c("Threshold 20%", "Yes"),
                     `Model 5` = c("Threshold 25%", "Yes"),
                     `Model 6` = c("Threshold 30%", "Yes"))
attr(offset_row, "position") <- c(1, 12)

modelsummary(list(nb_ratio,
                  nb_ratio_t10,
                  "Stops of Black Civilians" = nb_ratio_t15,
                  nb_ratio_t20,
                  nb_ratio_t25,
                  nb_ratio_t30),
             coef_omit = "(Intercept)|theta",
             coef_rename = c(black_ratio = "Black Racial Congruence",
                             percent10TRUE = "10% Threshold",
                             percent15TRUE = "15% Threshold",
                             percent20TRUE = "20% Threshold",
                             percent25TRUE = "25% Threshold",
                             percent30TRUE = "30% Threshold",
                             mean_years_worked_unit = "Years Worked In Unit (Mean)",
                             violent_cr_capita = "Violent Crime Per 10,000",
                             property_cr_capita = "Property Crime Per 10,000",
                             log_total_officers = "Log of the Total Number of Officers"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             gof_omit = "R2$|R2 Adj|R2 Within",
             exponentiate = T,
             output = file.path(dir, "table_stops-thresholds.txt"),
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                       "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

################################################################################
# Estimate regression models using only the thresholds
nb_only_ratio_t10 <-
    femlm(
        black_stops ~
            mean_years_worked_unit +
            percent10 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_only_ratio_t20 <-
    femlm(
        black_stops ~
            mean_years_worked_unit +
            percent20 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_only_ratio_t30 <-
    femlm(
        black_stops ~
            mean_years_worked_unit +
            percent30 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

offset_row <- tibble(term = c("", "Offset - Black Pop."),
                     `Model 1` = c("Threshold 10%", "Yes"),
                     `Model 2` = c("Threshold 20%", "Yes"),
                     `Model 3` = c("Threshold 30%", "Yes"))
attr(offset_row, "position") <- c(1, 12)

modelsummary(list(nb_only_ratio_t10,
                  "Stops of Black Civilians" = nb_only_ratio_t20,
                  nb_only_ratio_t30),
             coef_omit = "(Intercept)|theta",
             coef_rename = c(percent10TRUE = "10% Threshold",
                             percent20TRUE = "20% Threshold",
                             percent30TRUE = "30% Threshold",
                             mean_years_worked_unit = "Years Worked In Unit (Mean)",
                             violent_cr_capita = "Violent Crime Per 10,000",
                             property_cr_capita = "Property Crime Per 10,000",
                             log_total_officers = "Log of the Total Number of Officers"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             gof_omit = "R2$|R2 Adj|R2 Within",
             exponentiate = T,
             output = file.path(dir, "table_stops-only-thresholds.txt"),
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                       "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

################################################################################
# Estimate regression models interacting racial congruence and thresholds
nb_interact_ratio_t10 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent10 +
            black_ratio * percent10 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_interact_ratio_t20 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent20 +
            black_ratio * percent20 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

nb_interact_ratio_t30 <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            percent30 +
            black_ratio * percent30 +
            violent_cr_capita +
            property_cr_capita +
            log_total_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level)

offset_row <- tibble(term = c("", "Offset - Black Pop."),
                     `Model 1` = c("Threshold 10%", "Yes"),
                     `Model 2` = c("Threshold 20%", "Yes"),
                     `Model 3` = c("Threshold 30%", "Yes"))
attr(offset_row, "position") <- c(1, 12)

modelsummary(list(nb_interact_ratio_t10,
                  "Stops of Black Civilians" = nb_interact_ratio_t20,
                  nb_interact_ratio_t30),
             coef_omit = "(Intercept)|theta",
             coef_rename = c(percent10TRUE = "10% Threshold",
                             percent20TRUE = "20% Threshold",
                             percent30TRUE = "30% Threshold",
                             mean_years_worked_unit = "Years Worked In Unit (Mean)",
                             violent_cr_capita = "Violent Crime Per 10,000",
                             property_cr_capita = "Property Crime Per 10,000",
                             log_total_officers = "Log of the Total Number of Officers"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             gof_omit = "R2$|R2 Adj|R2 Within",
             exponentiate = T,
             output = file.path(dir, "table_stops-interact-thresholds.txt"),
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                       "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))
