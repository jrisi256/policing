library(here)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

################################################################################
dir <- here("New", "aggregate_level_outcomes", "paper_summer_2023")

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
# Appendix Figure A1
graph <-
    unit_level %>%
    group_by(unit) %>%
    summarise(white_stop_rate = median(white_stop_rate),
              black_stop_rate = median(black_stop_rate),
              hispanic_stop_rate = median(hispanic_stop_rate)) %>%
    pivot_longer(cols = matches("rate"),
                 names_to = "Race",
                 values_to = "Stops") %>%
    mutate(Race = case_when(Race == "white_stop_rate" ~ "White",
                            Race == "black_stop_rate" ~ "Black",
                            Race == "hispanic_stop_rate" ~ "Hisp.")) %>%
    group_by(unit) %>%
    mutate(max_match = Stops == max(Stops),
           max_match_black = if_else(max_match & Race == "Black", T, F),
           max_unit = any(max_match_black)) %>%
    ungroup()

sorting_var <-
    graph %>%
    filter(Race == "Black") %>%
    arrange(desc(max_match), desc(Stops))

graph <- graph %>% mutate(unit = factor(unit, levels = sorting_var$unit))

figa1 <-
    graph %>%
    ggplot(aes(x = Race, y = Stops)) +
    geom_bar(stat = "identity", aes(fill = max_unit)) +
    theme_bw() +
    labs(x = "Race",
         y = "Stopping Rate Per 10,000",
         title = "Median Stopping Rate By Race For Each Police Unit",
         fill = "Stopping Rate Highest For Black Individuals?") +
    facet_wrap(~unit, scales = "free_y") +
    theme(text = element_text(size = 12, family = "Times"),
          axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1),
          legend.title = element_text(size = 8),
          legend.text=element_text(size = 8))

ggsave(
    filename = "Figure A1.pdf", plot = figa1, path = dir,
    height = 8, width = 12
)
