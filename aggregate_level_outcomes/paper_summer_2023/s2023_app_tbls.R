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
# Estimate regression models across different levels of White residents
unit_level_list_white <-
    unit_level %>%
    distinct(unit, prcnt_civ_white) %>%
    mutate(white_percentile = as.numeric(ntile(prcnt_civ_white, 3))) %>%
    select(-prcnt_civ_white) %>%
    full_join(unit_level, by = "unit", multiple = "all") %>%
    group_split(white_percentile)

names_white <-
    unlist(map(unit_level_list_white,
               function(df) {df %>% pull(white_percentile) %>% unique()}))

names_white[names_white == 1] <- "Low White Pop."
names_white[names_white == 2] <- "Medium White Pop."
names_white[names_white == 3] <- "High White Pop."
names(unit_level_list_white) <- names_white

regression_groups_white_ratio <-
    map(unit_level_list_white,
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

################################################################################
# Table A1
offset_row <- tibble(term = c("Black Stops", "Offset - Black Pop."),
                     `Model 1` = c("", "Yes"),
                     `Model 2` = c("", "Yes"),
                     `Model 3` = c("", "Yes"))
attr(offset_row, "position") <- c(1, 10)

modelsummary(regression_groups_white_ratio,
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
             output = file.path(dir, "tableA1_white-terciles.txt"),
             add_rows = offset_row,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                 )
)

################################################################################
# Estimate regression models where dependent variable is arrests
nb_prcnt_arrest <-
    femlm(black_arrests ~ prcnt_officer_black + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_ratio_arrest <-
    femlm(black_arrests ~ black_ratio + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_prcnt_arrest_stopsiv <-
    femlm(black_arrests ~ prcnt_officer_black + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              black_stops + offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_ratio_arrest_stopsiv <-
    femlm(black_arrests ~ black_ratio + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              black_stops + offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

################################################################################
# Table A2
offset_row <- tibble(term = c("", "Offset - Black Pop."),
                     `Model 1` = c("Model 1A", "Yes"),
                     `Model 2` = c("Model 2A", "Yes"),
                     `Model 3` = c("Model 3A", "Yes"),
                     `Model 4` = c("Model 4A", "Yes"))
attr(offset_row, "position") <- c(1, 10)
rename = c(black_ratio = "Black Racial Congruence",
           prcnt_officer_black = "Percentage of Officers Who Are Black",
           mean_years_worked_unit = "Years Worked In Unit (Mean)",
           violent_cr_capita = "Violent Crime Per 10,000",
           property_cr_capita = "Property Crime Per 10,000",
           log_total_officers = "Log of the Total Number of Officers",
           black_stops = "Number of stops of Black civilians",
           nr_officer = "Total number of officers")
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
    output = file.path(dir, "tableA2_arrests.txt"),
    add_rows = offset_row,
    notes =
        c(
        "Standard Errors in parentheses. Coefficients are incident rate ratios.",
        "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
    )

################################################################################
# Estimate regression models where dependent variable is uses of force
nb_prcnt_force <-
    femlm(black_force ~ prcnt_officer_black + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_ratio_force <-
    femlm(black_force ~ black_ratio + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_prcnt_force_stopsiv <-
    femlm(black_force ~ prcnt_officer_black + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              black_stops + offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

nb_ratio_force_stopsiv <-
    femlm(black_force ~ black_ratio + mean_years_worked_unit +
              violent_cr_capita + property_cr_capita + nr_officer +
              black_stops + offset(log(black)) | unit + year,
          family = "negbin",
          data = unit_level)

################################################################################
# Table A3
offset_row <- tibble(term = c("", "Offset - Black Pop."),
                     `Model 1` = c("Model 5A", "Yes"),
                     `Model 2` = c("Model 6A", "Yes"),
                     `Model 3` = c("Model 7A", "Yes"),
                     `Model 4` = c("Model 8A", "Yes"))
attr(offset_row, "position") <- c(1, 9)
modelsummary(list(nb_prcnt_force,
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
             output = file.path(dir, "tableA3_force.txt"),
             add_rows = offset_row,
             notes =
                 c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
                   "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
                  )
             )
