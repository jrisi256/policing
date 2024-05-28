library(here)
library(dplyr)
library(readr)
library(purrr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data

dir <- here("aggregate_level_outcomes", "paper_summer_2023")

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

offset_row_halves <- tibble(term = c("Black Stops", "Offset - Black Pop."),
                            `Model 1` = c("", "Yes"),
                            `Model 2` = c("", "Yes"))
attr(offset_row_halves, "position") <- c(1, 10)

################################################################################
# Estimate regression models across different 2 levels of White residents
unit_level_list_white_halves <-
    unit_level %>%
    distinct(unit, prcnt_civ_white) %>%
    mutate(white_percentile = as.numeric(ntile(prcnt_civ_white, 2))) %>%
    select(-prcnt_civ_white) %>%
    full_join(unit_level, by = "unit", multiple = "all") %>%
    group_split(white_percentile)

names_white_halves <-
    unlist(map(unit_level_list_white_halves,
               function(df) {df %>% pull(white_percentile) %>% unique()}))

names_white_halves[names_white_halves == 1] <- "Lower Half - White Pop."
names_white_halves[names_white_halves == 2] <- "Upper Half - White Pop."
names(unit_level_list_white_halves) <- names_white_halves

regression_groups_white_ratio_halves <-
    map(unit_level_list_white_halves,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          black_ratio +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          log_total_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

regression_groups_white_prcnt_halves <-
    map(unit_level_list_white_halves,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          prcnt_officer_black +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          log_total_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

################################################################################
# White population by halves for percentage of Black officers
modelsummary(regression_groups_white_ratio_halves,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_white-halves-ratio.txt"),
             add_rows = offset_row_halves,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

################################################################################
# White population by halves for percentage of Black officers
modelsummary(regression_groups_white_prcnt_halves,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_white-halves-prcnt.txt"),
             add_rows = offset_row_halves,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

################################################################################
# Estimate regression models across 3 different levels of disadvantage
unit_level_list_dis_halves <-
    unit_level %>%
    distinct(unit, disadvantage) %>%
    mutate(disadvantage_percentile = as.numeric(ntile(disadvantage, 2))) %>%
    select(-disadvantage) %>%
    full_join(unit_level, by = "unit", multiple = "all") %>%
    group_split(disadvantage_percentile)

names_dis_halves <-
    unlist(map(unit_level_list_dis_halves,
               function(d) {d %>% pull(disadvantage_percentile) %>% unique()}))

names_dis_halves[names_dis_halves == 1] <- "Lower Half Disadvantage"
names_dis_halves[names_dis_halves == 2] <- "Upper Half Disadvantage"
names(unit_level_list_dis_halves) <- names_dis_halves

regression_groups_dis_ratio_halves <-
    map(unit_level_list_dis_halves,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          black_ratio +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          log_total_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

regression_groups_dis_prcnt_halves <-
    map(unit_level_list_dis_halves,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          prcnt_officer_black +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          log_total_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

################################################################################
# Disadvantage by halves for racial congruence
modelsummary(regression_groups_dis_ratio_halves,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_disadvantage-halves-ratio.txt"),
             add_rows = offset_row_halves,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

# Disadvantage by halves for percentage of Black officers
modelsummary(regression_groups_dis_prcnt_halves,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_disadvantage-halves-prcnt.txt"),
             add_rows = offset_row_halves,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)
