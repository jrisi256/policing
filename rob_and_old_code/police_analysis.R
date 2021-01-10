library(here)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(forcats)
library(lubridate)

unitHistory <-
    read_csv(here("..", "..", "police", "unified_data", "full-output", "unit-history", "unit-history.csv"))

complaintsAccused <-
    read_csv(here("..", "..", "police", "unified_data", "full-output", "complaints", "complaints-accused.csv"),
             col_types = cols(recc_finding = "c", recc_outcome = "c"))

complaintsComplaints <-
    read_csv(here("..", "..", "police", "unified_data", "full-output", "complaints", "complaints-complaints.csv"),
             col_types = cols(cr_id = "c", full_address = "c", incident_date = "c"))

complaints <-
    left_join(complaintsAccused,
              select(complaintsComplaints, cr_id, incident_date),
              by = "cr_id") %>%
    mutate(year = str_extract(incident_date, "^[0-9]{4}"),
           month = str_replace_all(str_extract(incident_date, "-[0-9]{2}-"), "-", ""),
           day = str_sub(str_extract(incident_date, "[0-9]{2}$")))

officerUnit <-
    full_join(select(unitHistory, UID, unit, unit_start_date, unit_end_date),
              select(complaints, UID, incident_date),
              by = "UID") %>%
    mutate(unit_start_date = ymd(unit_start_date),
           unit_end_date = ymd(unit_end_date),
           incident_date = ymd(incident_date),
           incident_year = year(incident_date),
           unit = as.character(unit)) %>%
    filter(!is.na(incident_date),
           incident_date >= unit_start_date,
           incident_date <= unit_end_date)

complaintsByUnit <- officerUnit %>% count(unit)
ggplot(complaintsByUnit, aes(x = fct_reorder(unit, n), y = n)) + geom_point() + geom_line(group = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 1000, 50), limits = c(0, 1000))

fltrComplaintsUnit <- complaintsByUnit %>% filter(n >= median(n)) %>% mutate(prcntRank = ntile(n, 10))
complaintsByUnitYear <-
    officerUnit %>%
    count(unit, incident_year) %>%
    right_join(select(fltrComplaintsUnit, unit, prcntRank), by = "unit") %>%
    filter(incident_year >= 1990, incident_year <= 2015)
ggplot(complaintsByUnitYear, aes(x = incident_year, y = n)) +
    geom_point() +
    geom_line(aes(group = unit, color = unit)) +
    facet_wrap(~prcntRank, nrow = 5, scales = "free_y") +
    theme_bw()



complaintsByYear <- complaints %>% count(year) %>% mutate(year = as.numeric(year))
complaintsByMonth <- complaints %>% count(month)
complaintsByDay <- complaints %>% count(day)
complaintsByMonthDay <- complaints %>% count(month, day) %>% mutate(monthDay = paste0(month, day))

ggplot(complaintsByYear, aes(x = year, y = n)) + geom_point() + geom_line(group = 1) + theme_bw()
ggplot(complaintsByMonth, aes(x = month, y = n)) + geom_point() + geom_line(group = 1) + theme_bw()
ggplot(complaintsByDay, aes(x = day, y = n)) + geom_point() + geom_line(group = 1) + theme_bw()
ggplot(complaintsByMonthDay, aes(x = monthDay, y = n)) + geom_point() + geom_line(group = 1, aes(color = month)) + theme_bw()

complaintsByOfficer <- complaints %>% count(UID)
ggplot(complaintsByOfficer, aes(x = n)) + geom_histogram(bins = 50) +
    scale_x_continuous(breaks = seq(0, 200, 10)) + theme_bw()
summary(complaintsByOfficer$n)
