library(dplyr)
library(ggplot2)
library(forcats)

ols_shifts <- readRDS(file = "ols_shifts.rds")
ols_stops <- readRDS(file = "ols_stops.rds")
ols_arrests <- readRDS(file = "ols_arrests.rds")

variables_shift <- c("White-Black", "White-Hispanic", "Black-Black",
                     "Black-Hispanic", "Hispanic-Hispanic", "Male-Female",
                     "Female-Female", "Years of Experience, Officer 1",
                     "Years of Experience, Officer 2",
                     "Years of Experience - Differential")

variables_stops_arrests <- c(variables_shift, "Number of Shared Shifts")

shift_results <-
    tibble(Coefficient = coef(ols_shifts)[-1],
           Variable = variables_shift,
           ci95 = 1.96 * summary(ols_shifts)$coefficients[,2][-1],
           outcome = "shifts") %>%
    mutate(Variable = fct_relevel(Variable,
                                  "Years of Experience - Differential",
                                  "Years of Experience, Officer 2",
                                  "Years of Experience, Officer 1",
                                  "Female-Female",
                                  "Male-Female",
                                  "Hispanic-Hispanic",
                                  "Black-Hispanic",
                                  "Black-Black",
                                  "White-Hispanic",
                                  "White-Black"))

ggplot(shift_results, aes(x = Coefficient, y = Variable)) +
    geom_point(size = 2) +
    geom_errorbar(aes(xmin = Coefficient - ci95,
                      xmax = Coefficient + ci95),
                  width = 0.5) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    theme(plot.title = element_text(size = 25),
          axis.title = element_text(size = 19),
          axis.text = element_text(size = 15)) +
    labs(title = "Coefficient Estimates for Number of Shifts Worked Together")

stops_arrests_results <-
    tibble(Coefficient = c(coef(ols_stops)[-1], coef(ols_arrests)[-1]),
           Variable = c(variables_stops_arrests, variables_stops_arrests),
           ci95 = c(1.96 * summary(ols_stops)$coefficients[,2][-1],
                    1.96 * summary(ols_arrests)$coefficients[,2][-1] ),
           outcome = c(rep("Stops", length(coef(ols_stops)[-1])),
                       rep("Arrests", length(coef(ols_arrests)[-1])))) %>%
    mutate(Variable = fct_relevel(Variable,
                                  "Years of Experience - Differential",
                                  "Years of Experience, Officer 2",
                                  "Years of Experience, Officer 1",
                                  "Female-Female",
                                  "Male-Female",
                                  "Number of Shared Shifts",
                                  "Hispanic-Hispanic",
                                  "Black-Hispanic",
                                  "Black-Black",
                                  "White-Hispanic",
                                  "White-Black"))

ggplot(stops_arrests_results, aes(x = Coefficient, y = Variable)) +
    geom_point(size = 2) +
    geom_errorbar(aes(xmin = Coefficient - ci95,
                      xmax = Coefficient + ci95),
                  width = 0.5) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    theme(plot.title = element_text(size = 25),
          axis.title = element_text(size = 19),
          axis.text = element_text(size = 15),
          strip.text.x = element_text(size = 15)) +
    facet_wrap(~outcome, scales = "free") +
    labs(title = "Coefficient Estimates for Number of Stops and Arrests Made Together")
