library(here)
library(readr)
library(dplyr)
library(ggplot2)

################################################################################
dir <- here("aggregate_level_outcomes", "paper_summer_2023")

# Read in data
unit_level <- read_csv(file.path(dir, "unit_level.csv"))

################################################################################
# Create figure 1, demonstrating racial congruence
fig1 <-
    unit_level %>%
    filter(month == "6") %>%
    ggplot(aes(x = prcnt_civ_black, y = prcnt_officer_black)) +
    geom_point(size = 0.5) +
    geom_abline() +
    facet_wrap(~year) +
    theme_bw() +
    labs(x = "Proportion of Population - Black",
         y = "Proportion of Police Unit - Black",
         title = "Proportion of the population that is Black vs. proportion of the police force that is Black") +
    theme(text = element_text(size = 5))

ggsave(filename = "fig1_racial-congruence.png", plot = fig1, path = dir)

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
# Create figs. 2a and 2b demonstrating between + within unit variation in stops
fig2a <-
    ggplot(unit_level, aes(x = date, y = black_stops)) +
    facet_wrap(~unit, scales = "free_y") +
    geom_point(size = 0.1) +
    theme_bw() +
    labs(x = "Date",
         y = "Number of Stops Of Black Civilians",
         title = "Number of Stops Within Each Unit Over Time") +
    theme(text = element_text(size = 5))

ggsave(filename = "fig2a_within-unit-stops.png", plot = fig2a, path = dir)

fig2b <-
    ggplot(unit_level, aes(x = date, y = black_stops)) +
    geom_point(size = 0.1, aes(color = unit)) +
    geom_line(linewidth = 0.25, aes(color = unit, group = unit)) +
    theme_bw() +
    labs(x = "Date",
         y = "Number of Stops Of Black Civilians",
         title = "Number of Stops Between Each Unit Over Time") +
    theme(text = element_text(size = 5))

ggsave(filename = "fig2b_between-unit-stops.png", plot = fig2b, path = dir)

################################################################################
# Figs. 3a and 3b, between + within unit variation in racial congruence
fig3a <-
    ggplot(unit_level, aes(x = date, y = black_ratio)) +
    facet_wrap(~unit, scales = "free_y") +
    geom_point(size = 0.1) +
    theme_bw() +
    labs(x = "Date",
         y = "Black Racial Congruence",
         title = "Black Racial Congruence Within Each Unit Over Time") +
    theme(text = element_text(size = 5))

ggsave(filename = "fig3a_within-racial-congr.png", plot = fig3a, path = dir)

fig3b <-
    ggplot(unit_level, aes(x = date, y = black_ratio)) +
    geom_point(size = 0.1, aes(color = unit)) +
    geom_line(linewidth = 0.25, aes(color = unit, group = unit)) +
    theme_bw() +
    labs(x = "Date", y = "Black Racial Congruence",
         title = "Black Racial Congruence Between Each Unit Over Time") +
    theme(text = element_text(size = 5))

ggsave(filename = "fig3b_between-racial-congr.png", plot = fig3b, path = dir)
