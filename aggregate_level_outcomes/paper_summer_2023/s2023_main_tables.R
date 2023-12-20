library(here)
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(fixest)
library(kableExtra)
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
# Table 1 for descriptive statistics
unit_level_descriptive_vars <-
    unit_level %>%
    select(`Total Number of Stops` = stops,
           `Stops of Black Civilians` = black_stops,
           `Stops of Black civilians per 10,000` = black_stop_rate,
           `Stops of White civiilians per 10,000` = white_stop_rate,
           `Stops of Hispanic civilians per 10,000` = hispanic_stop_rate,
           `Total Number of Officers` = nr_officer,
           `Total Number of Black Officers` = nr_officer_black,
           `Percent of Officers - Black` = prcnt_officer_black,
           `Total Population` = total_pop,
           `Black Population` = black,
           `Percent of Population - Black` = prcnt_civ_black,
           `Black Racial Congruence` = black_ratio,
           `Years Worked in Unit (Mean)` = mean_years_worked_unit,
           `Violent Crime Per 10,000 Individuals` = violent_cr_capita,
           `Property Crime Per 10,000 Individuals` = property_cr_capita) %>%
    rename() %>%
    as.data.frame()

datasummary(
    All(unit_level_descriptive_vars) ~ Mean + SD + Min + P25 + Median + P75 + Max,
    data = unit_level_descriptive_vars,
    output = file.path(dir, "table1_descriptive-table.txt")
)

################################################################################
# Models for % black and black stops with/without experience
unit_level_model_vars <-
    unit_level %>%
    select(year, unit, black_stops, prcnt_officer_black, black_ratio,
           violent_cr_capita, property_cr_capita, log_total_officers, black,
           mean_years_worked_unit, nr_officer)

nb_prcnt <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_prcnt_exp <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_prcnt_exp_no_log <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

################################################################################
# Models for black congruence and black stops with/without experience
nb_ratio <-
    femlm(
        black_stops ~
            black_ratio +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp_no_log <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officer +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

##################################################### Display results in a table
# Table 2
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 1", "Yes"),
        `Model 2` = c("Model 2", "Yes"),
        `Model 3` = c("Model 3", "Yes"),
        `Model 4` = c("Model 4", "Yes")
    )
attr(offset_row, "position") <- c(1, 9)
rename =
    c(black_ratio = "Black Racial Congruence",
      prcnt_officer_black = "Percentage of Officers Who Are Black",
      mean_years_worked_unit = "Years Worked In Unit (Mean)",
      violent_cr_capita = "Violent Crime Per 10,000",
      property_cr_capita = "Property Crime Per 10,000",
      log_total_officers = "Log of the Total Number of Officers",
      nr_officer = "Total number of officers"
     )

modelsummary(
    list(nb_ratio,
         "Stops of Black Civilians" = nb_prcnt,
         nb_ratio_exp,
         nb_prcnt_exp
        ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "table2_stops-aggregate.txt"),
    add_rows = offset_row,
    notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
              "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
             )
    )

modelsummary(
    list(nb_ratio_exp,
         "Stops of Black Civilians" = nb_ratio_exp_no_log,
         nb_prcnt_exp,
         nb_prcnt_exp_no_log
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(dir, "tableReviewer36_stops-aggregate.txt"),
    add_rows = offset_row,
    notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
              "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
)

################################################################################
# Make predictions using model 3 (racial congruence with experience)
nr_officers_ratio <-
    unit_level %>%
    select(unit, year, black_ratio, nr_officer_black) %>%
    group_by(unit, year) %>%
    mutate(rank = row_number(black_ratio)) %>%
    mutate(type = case_when(rank == min(rank) ~ "min",
                            rank == max(rank) ~ "max",
                            rank == 4 ~ "p25",
                            rank == 6 ~ "med",
                            rank == 9 ~ "p75",
                            T ~ "drop")) %>%
    ungroup() %>%
    filter(type != "drop") %>%
    select(-black_ratio, -rank)

new_data_ratio <-
    unit_level_model_vars %>%
    group_by(unit, year) %>%
    summarise(
        mean_years_worked_unit = mean(mean_years_worked_unit),
        violent_cr_capita = mean(violent_cr_capita),
        property_cr_capita = mean(property_cr_capita),
        nr_officer = mean(nr_officer),
        black = mean(black),
        min = min(black_ratio),
        max = max(black_ratio),
        med = median(black_ratio),
        p25 = quantile(black_ratio)[["25%"]],
        p75  = quantile(black_ratio)[["75%"]]
    ) %>%
    pivot_longer(
        c("min", "max", "med", "p25", "p75"),
        names_to = "type",
        values_to = "black_ratio") %>%
    ungroup() %>%
    full_join(nr_officers_ratio, by = c("unit", "year", "type"))

predict_results_ratio <-
    new_data_ratio %>%
    mutate(predicted_black_stops =
               predict(nb_ratio_exp, newdata = new_data_ratio) * black,
           type = factor(type, levels = c("min", "p25", "med", "p75", "max")),
           variable = "Black Racial Congruence")

predict_results_diff <-
    predict_results_ratio %>%
    select(unit, year, type, black_ratio, nr_officer_black,
           predicted_black_stops) %>%
    filter(type == "min" | type == "max") %>%
    pivot_wider(id_cols = c("unit", "year"),
                names_from = type,
                values_from =
                    c("predicted_black_stops",
                      "black_ratio",
                      "nr_officer_black"
                      )
                ) %>%
    mutate(decrease_stops = predicted_black_stops_min - predicted_black_stops_max,
           chg_officer = nr_officer_black_max - nr_officer_black_min)

predict_table <-
    predict_results_diff %>%
    mutate(unit = as.numeric(unit)) %>%
    arrange(unit) %>%
    filter(year == 2014) %>%
    select(-year) %>%
    mutate(across(matches("stops"), ~round(.x)),
           across(matches("ratio"), ~round(.x, 2))) %>%
    unite("predicted_stops",
          predicted_black_stops_min:predicted_black_stops_max,
          sep = " to ") %>%
    unite("black_ratio",
          black_ratio_min:black_ratio_max,
          sep = " to ") %>%
    unite("nr_officer_black",
          nr_officer_black_min:nr_officer_black_max,
          sep = " to ")

kable(predict_table,
      format = "simple",
      col.names = c("Unit",
                    "Change - Pred. Black Stops",
                    "Min to Max, Racial Con.",
                    "Change - Black Officers",
                    "Diff - Stops",
                    "Diff - Officers"))
