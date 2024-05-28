library(dplyr)
library(tidyr)
library(lubridate)

##################################################################
##            Describe character and logical columns            ##
##################################################################
describe_columns_c_l <-
    function(df, cut = "max", nmax = 5, nmin = 5) {
        df <-
            df %>%
            # Convert data from wide to long
            pivot_longer(
                cols = everything(),
                names_to = "variable",
                values_to = "value",
                values_transform = as.character
            ) %>%
            # Count the number of times each value of a variable appears
            count(variable, value, name = "nr") %>%
            group_by(variable) %>%
            mutate(
                prop = round(nr / sum(nr), digits = 3),
                nr_unique = length(unique(value))
            )
        
        # Return largest, smallest, or both largest and smallest values
        if (cut == "max") {
            df <- df %>% slice_max(nr, n = nmax, with_ties = F)
        } else if (cut == "min") {
            df <- df %>% slice_min(nr, n = nmin, with_ties = F)
        } else if (cut == "min_max") {
            df_max <- df %>% slice_max(nr, n = nmax, with_ties = F)
            df_min <- df %>% slice_min(nr, n = nmin, with_ties = F)
            df <- bind_rows(df_max, df_min) %>% arrange(variable)
        }
        
        df <- df %>% ungroup()
    }

#################################################################
##                     Describe ID columns                     ##
#################################################################
describe_columns_id <-
    function(df) {
        df <-
            df %>%
            # Convert data from wide to long
            pivot_longer(
                cols = everything(),
                names_to = "variable",
                values_to = "value",
                values_transform = as.character
            ) %>%
            group_by(variable) %>%
            # Count number of unique values for the ID and number missing
            summarise(
                nr_rows = n(),
                nr_unique = length(unique(value)),
                nr_missing = sum(is.na(value)),
                prop_missing = round(sum(is.na(value)) / n(), digits = 3)
            )
    }

##################################################################
##                   Describe numeric columns                   ##
##################################################################
describe_columns_n <-
    function(df) {
        df <-
            df %>%
            # Convert data from wide to long
            pivot_longer(
                cols = everything(), names_to = "variable", values_to = "value"
            ) %>%
            group_by(variable) %>%
            # Describe distribution of data
            summarise(
                mean = round(mean(value, na.rm = T), digits = 3),
                sd = round(sd(value, na.rm = T), digits = 3),
                min = round(min(value, na.rm = T), digits = 3),
                p25 =
                    round(quantile(value, probs = 0.25, na.rm = T)[["25%"]], digits = 3),
                median = round(median(value, na.rm = T), digits = 3),
                p75 =
                    round(quantile(value, probs = 0.75, na.rm = T)[["75%"]], digits = 3),
                max = round(max(value, na.rm = T), digits = 3),
                iqr = round(IQR(value, na.rm = T), digits = 3),
                mad = round(mad(value, na.rm = T), digits = 3),
                nr_missing = sum(is.na(value)),
                prop_missing = round(sum(is.na(value)) / n(), digits = 3)
            ) %>%
            ungroup()
    }


######################################################################
##  Describe date and date time variables as if they were numbers.  ##
######################################################################
describe_columns_date_num <- function(df) {
    df %>%
        pivot_longer(
            cols = everything(), names_to = "variable", values_to = "value"
        ) %>%
        group_by(variable) %>%
        summarise(
            min = min(value, na.rm = T),
            p25 = quantile(value, probs = 0.25, na.rm = T, type = 1)[["25%"]],
            median = median(value, na.rm = T),
            p75 = quantile(value, probs = 0.75, na.rm = T, type = 1)[["75%"]],
            max = max(value, na.rm = T),
            nr_missing = sum(is.na(value)),
            prop_missing = round(sum(is.na(value)) / n(), digits = 3)
        )
}

###############################################################################
##  Describe month and year variables as if they were categorical variables. ##
###############################################################################
describe_columns_month_year_category <- function(df) {
    df %>%
        pivot_longer(
            cols = everything(), names_to = "variable", values_to = "date"
        ) %>%
        # Describe the days, month, year, days of week, and hour of day.
        mutate(
            month = month(date),
            year = year(date)
        ) %>%
        select(-date) %>%
        # Convert data from wide to long.
        pivot_longer(
            cols = month:year,
            names_to = "unit_of_time",
            values_to = "value"
        ) %>%
        # Count number of times an observation happens during each period of time.
        count(variable, unit_of_time, value) %>%
        group_by(variable, unit_of_time) %>%
        mutate(prop = round(n / sum(n), digits = 3)) %>%
        ungroup()
}

######################################################################
##  Describe date variables as if they were categorical variables.  ##
######################################################################
describe_columns_date_category <- function(df) {
    df %>%
        pivot_longer(
            cols = everything(), names_to = "variable", values_to = "date"
        ) %>%
        # Describe the days, month, year, days of week, and hour of day.
        mutate(
            day = mday(date),
            month = month(date),
            year = year(date)
        ) %>%
        select(-date) %>%
        # Convert data from wide to long.
        pivot_longer(
            cols = day:year,
            names_to = "unit_of_time",
            values_to = "value"
        ) %>%
        # Count number of times an observation happens during each period of time.
        count(variable, unit_of_time, value) %>%
        group_by(variable, unit_of_time) %>%
        mutate(prop = round(n / sum(n), digits = 3)) %>%
        ungroup()
}

#########################################################################
##  Describe datatime variables as if they were categorical variables  ##
#########################################################################
describe_columns_datetime_category <- function(df) {
    df %>%
        pivot_longer(
            cols = everything(), names_to = "variable", values_to = "date"
        ) %>%
        # Describe the days, month, year, days of week, and hour of day.
        mutate(
            day = mday(date),
            month = month(date),
            year = year(date),
            day_of_week = wday(date),
            hour = hour(round_date(date, unit = "hour"))
        ) %>%
        select(-date) %>%
        # Convert data from wide to long.
        pivot_longer(
            cols = day:hour,
            names_to = "unit_of_time",
            values_to = "value"
        ) %>%
        # Count number of times an observation happens during each period of time.
        count(variable, unit_of_time, value) %>%
        group_by(variable, unit_of_time) %>%
        mutate(prop = round(n / sum(n), digits = 3)) %>%
        ungroup()
}
