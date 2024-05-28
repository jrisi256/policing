library(here)
library(dplyr)
library(readr)
library(purrr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data
dir <- here("New", "aggregate_level_outcomes", "paper_summer_2023")

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

offset_row_tercile <- tibble(term = c("Black Stops", "Offset - Black Pop."),
                             `Model 1` = c("", "Yes"),
                             `Model 2` = c("", "Yes"),
                             `Model 3` = c("", "Yes"))
attr(offset_row_tercile, "position") <- c(1, 10)

################################################################################
# Estimate regression models across different 3 levels of White residents
unit_level_list_white_tercile <-
    unit_level %>%
    distinct(unit, prcnt_civ_white) %>%
    mutate(white_percentile = as.numeric(ntile(prcnt_civ_white, 3))) %>%
    select(-prcnt_civ_white) %>%
    full_join(unit_level, by = "unit", multiple = "all") %>%
    group_split(white_percentile)

names_white_tercile <-
    unlist(map(unit_level_list_white_tercile,
               function(df) {df %>% pull(white_percentile) %>% unique()}))

names_white_tercile[names_white_tercile == 1] <- "Low White Pop."
names_white_tercile[names_white_tercile == 2] <- "Medium White Pop."
names_white_tercile[names_white_tercile == 3] <- "High White Pop."
names(unit_level_list_white_tercile) <- names_white_tercile

regression_groups_white_prcnt_tercile <-
    map(unit_level_list_white_tercile,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          prcnt_officer_black +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          nr_officer +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

################################################################################
# Table A1 but with percent black instead of racial congruence
modelsummary(regression_groups_white_prcnt_tercile,
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
             output = file.path(dir, "table_white-terciles-prcnt.txt"),
             add_rows = offset_row_tercile,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

################################################################################
# Estimate regression models across 3 different levels of disadvantage
unit_level_list_pvrty_tercile <-
    unit_level %>%
    distinct(unit, prcnt_poverty) %>%
    mutate(poverty_percentile = as.numeric(ntile(prcnt_poverty, 3))) %>%
    select(-prcnt_poverty) %>%
    full_join(unit_level, by = "unit", multiple = "all") %>%
    group_split(poverty_percentile)

names_pvrty_tercile <-
    unlist(map(unit_level_list_pvrty_tercile,
               function(d) {d %>% pull(poverty_percentile) %>% unique()}))

names_pvrty_tercile[names_pvrty_tercile == 1] <- "Low Poverty"
names_pvrty_tercile[names_pvrty_tercile == 2] <- "Medium Poverty"
names_pvrty_tercile[names_pvrty_tercile == 3] <- "High Poverty"
names(unit_level_list_pvrty_tercile) <- names_pvrty_tercile

regression_groups_pvrty_ratio_tercile <-
    map(unit_level_list_pvrty_tercile,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          black_ratio +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          nr_officer +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

regression_groups_pvrty_prcnt_tercile <-
    map(unit_level_list_pvrty_tercile,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          prcnt_officer_black +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          nr_officer +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        })

################################################################################
# Disadvantage by tercile for racial congruence
modelsummary(regression_groups_pvrty_ratio_tercile,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers",
                   nr_officer = "Total number of officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             #fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_poverty-terciles-ratio.txt"),
             add_rows = offset_row_tercile,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

# Disadvantage by tercile for percentage of Black officers
modelsummary(regression_groups_pvrty_prcnt_tercile,
             coef_omit = "(Intercept)|theta",
             coef_rename =
                 c(black_ratio = "Black Racial Congruence",
                   mean_years_worked_unit = "Years Worked In Unit (Mean)",
                   prcnt_officer_black = "Percentage of Officers Who Are Black",
                   violent_cr_capita = "Violent Crime Per 10,000",
                   property_cr_capita = "Property Crime Per 10,000",
                   log_total_officers = "Log of the Total Number of Officers",
                   nr_officer = "Total number of officers"
                 ),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             fmt = 2,
             stars = T,
             gof_omit = "R2|RMSE",
             exponentiate = T,
             output = file.path(dir, "table_poverty-terciles-prcnt.txt"),
             add_rows = offset_row_tercile,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)
