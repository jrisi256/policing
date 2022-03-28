library(here)
library(skimr)
library(stringr)

my_read_csv <- function(file_string, path) {
    
    if(file_string == "officers.csv") {
        read_csv(here(path, file_string)) %>%
            mutate(officer_race = as.factor(officer_race),
                   officer_gender = as.factor(officer_gender),
                   officer_id = as.character(officer_id),
                   spanish = as.logical(spanish))
        
    } else if(file_string == "force.csv") {
        read_csv(here(path, file_string)) %>%
            mutate(civ.race = as.factor(civ.race),
                   civ.gender = as.factor(civ.gender),
                   civilian_race_short = as.factor(civilian_race_short),
                   district = as.factor(district),
                   civ.injured = as.logical(civ.injured),
                   force_id = as.character(force_id),
                   officer_id = as.character(officer_id),
                   lat = as.character(lat),
                   lon = as.character(lon))
        
    } else if(file_string == "assignments.csv") {
        read_csv(here(path, file_string)) %>%
            mutate(rank = as.factor(rank),
                   weekday = as.factor(weekday),
                   officer_id = as.character(officer_id),
                   unit = as.factor(unit),
                   shift = as.factor(shift))
        
    } else if(file_string == "stops.csv") {
        read_csv(here(path, file_string)) %>%
            mutate(stop_type = as.factor(stop_type),
                   contact_type = as.factor(contact_type),
                   civ.race = as.factor(civ.race),
                   civ.gender = as.factor(civ.gender),
                   civilian_race_short = as.factor(civilian_race_short),
                   stop_id = as.character(stop_id),
                   district = as.factor(district),
                   po_first = as.logical(po_first),
                   lat = as.character(lat),
                   lon = as.character(lon),
                   officer_id = as.character(officer_id))
        
    } else if(file_string == "arrests.csv") {
        read_csv(here(path, file_string)) %>%
            mutate(crime_code = as.factor(crime_code),
                   civ.race = as.factor(civ.race),
                   civ.gender = as.factor(civ.gender),
                   civilian_race_short = as.factor(civilian_race_short),
                   lat = as.character(lat),
                   lon = as.character(lon),
                   district = as.factor(district),
                   arrest_id = as.character(arrest_id))
    }
}

get_percentages <- function(factor) {
    percentage_table <- sort(prop.table(table(factor)), decreasing = T)
    values <- sprintf("%.3f", percentage_table[1:min(c(length(percentage_table), 4))])
    names <- str_sub(names(percentage_table[1:min(c(length(percentage_table), 4))]), 1, 3)
    str_c(names, values, sep = ": ", collapse = ", ")
}

my_skim_pct <-
    skim_with(numeric = sfl(iqr = ~ IQR(., na.rm = T), hist = NULL),
              factor = sfl(pct = get_percentages,
                           top_counts = ~top_counts(.,
                                                    max_char = 3,
                                                    max_levels = 4)))

my_skim <-
    skim_with(numeric = sfl(iqr = ~ IQR(., na.rm = T), hist = NULL),
              factor = sfl(top_counts = ~top_counts(.,
                                                    max_char = 3,
                                                    max_levels = 4)))
