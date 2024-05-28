library(here)
library(dplyr)
library(modelsummary)

offset_row <- tibble(term = c("", "FE - Day of the Week", "FE - Month/Year", "FE - Shift Timing", "FE - Police District"),
                     `Model 1` = c("All Officers", "X", "X", "X", "X"),
                     `Model 2` = c("White Officers Only", "X", "X", "X", "X"),
                     `Model 3` = c("Black Officers Only", "X", "X", "X", "X"),
                     `Model 4` = c("Hispanic Officers Only", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

stops_ols <- readRDS(here("create-officer-assignments", "stops_ols.rds"))
white_stops <- readRDS(here("create-officer-assignments", "white_stops.rds"))
black_stops <- readRDS(here("create-officer-assignments", "black_stops.rds"))
hisp_stops <- readRDS(here("create-officer-assignments", "hisp_stops.rds"))
modelsummary(list(" " = stops_ols, "Dependent Variable - Stops" = white_stops, " " = black_stops, " " = hisp_stops),
             coef_rename = c(officer_black_demean = "Officer Race/Ethnicity - Black",
                             officer_hisp_demean = "Officer Race/Ethniciy - Hispanic",
                             officer_female_demean = "Officer Sex - Female",
                             officer_spanish_demean = "Officer Speaks Spanish",
                             officer_exp_demean = "Officer Experience (Years)",
                             officer_exp_demean_sq = "Officer Experience Squared (Years)",
                             prcnt_officer_black_demean = "Percent of shift that is Black",
                             prcnt_officer_hisp_demean = "Percent of shift that is Hispanic",
                             n_officer_demean = "Number of other officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             output = "stops.html",
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

arrests_ols <- readRDS(here("create-officer-assignments", "arrests_ols.rds"))
white_arrests <- readRDS(here("create-officer-assignments", "white_arrests.rds"))
black_arrests <- readRDS(here("create-officer-assignments", "black_arrests.rds"))
hisp_arrests <- readRDS(here("create-officer-assignments", "hisp_arrests.rds"))
modelsummary(list(" " = arrests_ols, "Dependent Variable - Arrests" = white_arrests, " " = black_arrests, " " = hisp_arrests),
             coef_rename = c(officer_black_demean = "Officer Race/Ethnicity - Black",
                             officer_hisp_demean = "Officer Race/Ethniciy - Hispanic",
                             officer_female_demean = "Officer Sex - Female",
                             officer_spanish_demean = "Officer Speaks Spanish",
                             officer_exp_demean = "Officer Experience (Years)",
                             officer_exp_demean_sq = "Officer Experience Squared (Years)",
                             prcnt_officer_black_demean = "Percent of shift that is Black",
                             prcnt_officer_hisp_demean = "Percent of shift that is Hispanic",
                             n_officer_demean = "Number of other officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             output = "arrests.html",
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))

force_ols <- readRDS(here("create-officer-assignments", "force_ols.rds"))
white_force <- readRDS(here("create-officer-assignments", "white_force.rds"))
black_force <- readRDS(here("create-officer-assignments", "black_force.rds"))
hisp_force <- readRDS(here("create-officer-assignments", "hisp_force.rds"))
modelsummary(list(" " = force_ols, "Dependent Variable - Uses of Force" = white_force, " " = black_force, " " = hisp_force),
             coef_rename = c(officer_black_demean = "Officer Race/Ethnicity - Black",
                             officer_hisp_demean = "Officer Race/Ethniciy - Hispanic",
                             officer_female_demean = "Officer Sex - Female",
                             officer_spanish_demean = "Officer Speaks Spanish",
                             officer_exp_demean = "Officer Experience (Years)",
                             officer_exp_demean_sq = "Officer Experience Squared (Years)",
                             prcnt_officer_black_demean = "Percent of shift that is Black",
                             prcnt_officer_hisp_demean = "Percent of shift that is Hispanic",
                             n_officer_demean = "Number of other officers on shift"),
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             stars = T,
             output = "force.html",
             add_rows = offset_row,
             notes = c("Standard Errors in parentheses.",
                       "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"))
