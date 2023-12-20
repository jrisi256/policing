library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fixest)
library(lubridate)
library(modelsummary)

################################################################################
# Read in individual-level data
dir <- here("aggregate_level_outcomes", "paper_summer_2023")

# Read in shift assignments (independent variables)
shift_assignments <- read_csv(file.path(dir, "officers_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <-
    read_csv(file.path(dir, "outcomes_ba_max.csv")) %>%
    mutate(stops_sus_tr = stops_suspicious + stops_traffic,
           stops_sus_tr_lo = stops_suspicious + stops_traffic + stops_loitering)

# join outcomes with independent variables and aggregate to the unit-month
types_of_stops <-
    shift_assignments %>%
    full_join(outcomes) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    filter(year != 2012 & year != 2016) %>%
    filter(unit != 13 & unit != 21 & unit != 23) %>%
    group_by(unit, year, month) %>%
    summarise(nr_stops_suspicious = sum(stops_suspicious, na.rm = T),
              nr_stops_other = sum(stops_other, na.rm = T),
              nr_stops_traffic = sum(stops_traffic, na.rm = T),
              nr_stops_drug = sum(stops_drug, na.rm = T),
              nr_stops_loitering = sum(stops_loitering, na.rm = T),
              nr_stops_sus_tr = sum(stops_sus_tr, na.rm = T),
              nr_stops_sus_tr_lo = sum(stops_sus_tr_lo, na.rm = T)) %>%
    ungroup()

# Read in unit level variables and join with new outcome variables
unit_level <-
    read_csv(file.path(dir, "unit_level.csv")) %>%
    full_join(types_of_stops) %>%
    mutate(prcnt_officer_black = prcnt_officer_black * 100,
           property_cr_capita = property_cr * 10000 / total_pop,
           violent_cr_capita = violent_cr * 10000 /  total_pop,
           white_stop_rate = round(white_stops / white * 10000),
           hispanic_stop_rate = round(hispanic_stops / black * 10000),
           mean_years_worked_unit = mean_months_worked_unit / 12,
           year = as.character(year),
           unit = as.character(unit))

################################################################################
# Estimate regression models for each type of stop
unit_level_list <-
    unit_level %>%
    pivot_longer(
        cols = matches("^stops$|black_stops|nr_stops"),
        names_to = "group",
        values_to = "dep_var"
    ) %>%
    group_split(group)

names <- map_chr(unit_level_list, function(df) {unique(df[["group"]])})
names(unit_level_list) <- names

unit_level_list <-
    pmap(list(unit_level_list, names(unit_level_list)),
         function(df, dep_var_name) {

             df %>%
                 mutate(dep_var_name = dep_var_name,
                        offset_var =
                            if_else(dep_var_name == "black_stops",
                                    black,
                                    total_pop
                                    )
                        )
         })

regression_groups <-
    map(unit_level_list,
        function(df) {
            femlm(dep_var ~
                      black_ratio +
                      mean_years_worked_unit +
                      violent_cr_capita +
                      property_cr_capita +
                      nr_officer | unit + year,
                  offset  = log(df$offset_var),
                  family = "negbin",
                  data = df)
        }
    )

offset_row <- tibble(term = c("", "Offset"),
                     `Model 1` = c(names(regression_groups)[1], "Black Pop."),
                     `Model 2` = c(names(regression_groups)[2], "Total Pop."),
                     `Model 3` = c(names(regression_groups)[3], "Total Pop."),
                     `Model 4` = c(names(regression_groups)[4], "Total Pop."),
                     `Model 5` = c(names(regression_groups)[5], "Total Pop."),
                     `Model 6` = c(names(regression_groups)[6], "Total Pop."),
                     `Model 7` = c(names(regression_groups)[7], "Total Pop."),
                     `Model 8` = c(names(regression_groups)[8], "Total Pop."),
                     `Model 9` = c(names(regression_groups)[9], "Total Pop."))
attr(offset_row, "position") <- c(1, 10)

modelsummary(regression_groups,
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
             output = file.path(dir, "table_different-stop-types-aggregate.txt"),
             add_rows = offset_row,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
             )
