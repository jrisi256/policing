library(here)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

incarceration <-
    read_csv(here("..", "..", "Downloads", "incarceration_trends.csv")) %>%
    mutate(fips = if_else(nchar(fips) == 4,
                          paste0("0", fips),
                          as.character(fips))) %>%
    select(year, fips, state, county_name, county_name, total_pop, total_pop_15to64,
           total_jail_pop, total_prison_pop, total_jail_adm, total_jail_pop_dcrp,
           total_jail_adm_dcrp, total_prison_pop, total_prison_adm, total_jail_pop_rate,
           total_prison_pop_rate, total_prison_adm_rate)

deaths <-
    read_tsv(here("..", "..", "Downloads", "deaths.txt")) %>%
    select(-Notes, -`Year Code`, -County)

final_df <-
    inner_join(incarceration, deaths,
               by = c("fips" = "County Code", "year" = "Year")) %>%
    filter(!(`Age Adjusted Rate` %in% c("Unreliable", "Suppressed", "Missing"))) %>%
    filter(!is.na(total_prison_pop_rate), !is.na(total_jail_pop_rate)) %>%
    select(fips, year, `Age Adjusted Rate`, total_jail_pop_rate, total_prison_pop_rate) %>%
    mutate(age_adjusted_rate = as.numeric(`Age Adjusted Rate`)) %>%
    select(-`Age Adjusted Rate`) %>%
    mutate(total_jail_pop_rate_std = (total_jail_pop_rate - mean(total_jail_pop_rate)) / sd(total_jail_pop_rate),
           total_prison_pop_rate_std = (total_prison_pop_rate - mean(total_prison_pop_rate)) / sd(total_prison_pop_rate),
           log_age_adjusted_rate = log(age_adjusted_rate))

model <- lm(log_age_adjusted_rate ~ total_jail_pop_rate_std + total_prison_pop_rate_std +
                fips + as.factor(year),
            data = final_df)

ggplot(final_df, aes(x = total_prison_pop_rate, y = age_adjusted_rate)) +
    geom_point() +
    theme_bw()

ggplot(final_df, aes(x = total_jail_pop_rate, y = age_adjusted_rate)) +
    geom_point() +
    theme_bw()
