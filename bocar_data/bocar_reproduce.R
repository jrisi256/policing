library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(data.table)

############################################## Read in officers

# Read in officers
officers <- read_csv(here("bocar_data", "officers.csv"))

# Read in beat assignments
assignments <-
    read_csv(here("bocar_data", "assignments.csv"))

assignmentsDate <- assignments %>% mutate(date = ymd(date), month = ymd(month))

################################################ Reproduce their checks
officerRankProp <- round(sort(prop.table(table(assignments$rank))), 4)
officerRaceProp <-
    officers %>%
    filter(officer_id %in% assignments$officer_id) %>%
    select(officer_race) %>%
    table() %>% prop.table() %>% sort(decreasing = T) %>% cumsum() %>% as.matrix()

################################################# Filter and merge
# Only keep police officer assignments
assignmentsFltr <- assignmentsDate %>% filter(rank == "POLICE OFFICER")

# only keep black, white, and Hispanic police officers
officersRace <-
    officers %>%
    filter(officer_race %in% c("officer_black", "officer_white", "officer_hisp"))

# Merge beat assignments to officer traits, drop officers w/ no assignments
assignmentOfficer <-
    assignmentsFltr %>%
    inner_join(select(officersRace, -appointed_month), by = "officer_id")

# If an assignment doesn't match, it's because the assignment was for an officer
# who isn't black, Hispanic, or white
a <- anti_join(assignmentsFltr, officersRace, by = "officer_id")
b <- filter(officers, officer_id %in% a$officer_id)
table(b$officer_race)

# If an officer doesn't match, it's either because of the officer's rank OR
# because the officer didn't have any assignments during this time period
# Only 5% of officers don't match because of their rank.
a <- anti_join(officersRace, assignmentsFltr, by = "officer_id")
b <- filter(assignments, officer_id %in% a$officer_id)
length(unique(b$officer_id)) / nrow(a)
b2 <- filter(assignmentsFltr, officer_id %in% a$officer_id)

############################################### investigating beat assignments
# number of unique patrol tasks
length(unique(assignmentOfficer$beat_assigned))

# number of shifts assigned to beat 1431 over the entire length of the data
nrshifts <- nrow(assignmentOfficer %>% filter(grepl("1431", beat_assigned)))

# Regular vs. relief shifts
table(assignmentOfficer %>%
          filter(grepl("1431", beat_assigned)) %>%
          select(beat_assigned))

###### Desk duty
# First remove all non-numeric characters
dd <- gsub("[^0-9]", "", assignmentOfficer$beat_assigned)

# It would appear as if all desk duties end in 02
avgDd <-
    mean(if_else(nchar(dd) == 4,
                 substr(dd, 3, 4) == "02",
                 substr(dd, 2, 3) == "02"))

# shift length
assignmentOfficer <-
    assignmentOfficer %>%
    mutate(shiftlength = end_time - start_time)

shiftlength.tab <- table(round(assignmentOfficer$shiftlength, 1))

# Graph the changing of the shifts for the 1431 beat
graphBeat <-
    assignmentOfficer %>%
    filter(beat_assigned == "1431" | beat_assigned == "1431R") %>%
    select(start_time, end_time, shift, beat_assigned) %>%
    pivot_longer(cols = c("start_time", "end_time"),
                          names_to = "start_or_end",
                          values_to = "time")

ggplot(graphBeat, aes(x = time)) +
    geom_density(aes(color = paste0(shift, beat_assigned, start_or_end))) +
    facet_wrap(~shift+beat_assigned, nrow = 1) +
    theme_bw()

# Average shift lengths
assignmentOfficer %>%
    filter(beat_assigned == "1431") %$%
    mean(shiftlength, na.rm = T)

assignmentOfficer %>%
    filter(beat_assigned == "1431R") %$%
    mean(shiftlength, na.rm = T)

assignmentOfficer %>%
    filter(beat_assigned == "1431") %$%
    table(shift)

assignmentOfficer %>%
    filter(beat_assigned == "1431R") %$%
    table(shift)

prop.table(shiftlength.tab)[c("9", "8.5", "8")]

#  For each shift... something to do with subtracting the mean
assignments.shiftlength <- demean.by.group(
    data.table(assignmentOfficer),
    id = 'officer_id',
    fe = c('beat_assigned', 'weekday', 'shift', 'month'),
    ds = 'officer_race',
    ys = 'shiftlength'
)

assignmentsShiftLength <-
    assignmentOfficer %>%
    select(c("officer_id", "beat_assigned", "weekday", "shift", "month",
             "officer_race", "shiftlength")) %>%
    filter(across(c("officer_id", "beat_assigned", "weekday", "shift", "month",
                    "officer_race", "shiftlength"),
           ~!is.na(.x))) %>%
    group_by(beat_assigned, weekday, shift, month) %>%
    mutate(meanShiftLength = mean(shiftlength),
           centeredShiftLength = shiftlength - meanShiftLength,
           var = 1,
           officerRace = officer_race) %>%
    ungroup() %>%
    mutate(id = row_number()) %>%
    pivot_wider(names_from = officer_race,
                values_from = var,
                values_fill = 0) %>%
    select(-id) %>%
    group_by(beat_assigned, weekday, shift, month) %>%
    mutate(officer_white = officer_white - mean(officer_white),
           officer_black = officer_black - mean(officer_black),
           officer_hisp = officer_hisp - mean(officer_hisp)) %>%
    ungroup()

# Black officers have very slightly shorter shifts
modelShiftLength <- lm(centeredShiftLength ~ officer_black + officer_hisp,
                       assignmentsShiftLength)

# Write out results so we don't have to do this pipeline every time
write_csv(assignmentOfficer, here("bocar_data", "assignmentsOfficer.csv"))
