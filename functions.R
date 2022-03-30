library(readr)
library(dplyr)
library(tidyr)
library(stringr)

my_read_csv <- function(file_path) {
    
    if(str_detect(file_path, "officers")) {
        read_csv(file_path,
                 col_types = list(officer_race = "f",
                                  officer_gender = "f",
                                  officer_id = "c",
                                  spanish = "l"))
        
    } else if(str_detect(file_path, "force")) {
        read_csv(file_path,
                 col_types = list(civ.race = "f",
                                  civ.gender = "f",
                                  civilian_race_short = "f",
                                  district = "f",
                                  civ.injured = "l",
                                  force_id = "c",
                                  officer_id = "c",
                                  lat = "c",
                                  lon = "c"))
        
    } else if(str_detect(file_path, "assignments")) {
        read_csv(file_path,
                 col_types = list(rank = "f",
                                  weekday = "f",
                                  officer_id = "c",
                                  unit = "f",
                                  shift = "f"))
        
    } else if(str_detect(file_path, "stops")) {
        read_csv(file_path,
                 col_types = list(stop_type = "f",
                                  contact_type = "f",
                                  civ.race = "f",
                                  civ.gender = "f",
                                  civilian_race_short = "f",
                                  stop_id = "c",
                                  district = "f",
                                  po_first = "l",
                                  lat = "c",
                                  lon = "c",
                                  officer_id = "c"))
        
    } else if(str_detect(file_path, "arrests")) {
        read_csv(file_path,
                 col_types = list(crime_code = "f",
                                  civ.race = "f",
                                  civ.gender = "f",
                                  civilian_race_short = "f",
                                  lat = "c",
                                  lon = "c",
                                  district = "f",
                                  arrest_id = "c",
                                  officer_id = "c"))
    }
}

GetSummaryCol <- function(df, col) {
    
    if(is.numeric(df[[col]])) {
        
        q <- quantile(df[[col]], na.rm = T)
        
        df %>%
            transmute(col = col,
                      min = min(.data[[col]], na.rm = T),
                      first_q = q[["25%"]],
                      med = median(.data[[col]], na.rm = T),
                      mean = mean(.data[[col]], na.rm = T),
                      third_q = q[["75%"]],
                      max = max(.data[[col]], na.rm = T),
                      sd = sd(.data[[col]], na.rm = T),
                      iqr = IQR(.data[[col]], na.rm = T),
                      mad = mad(.data[[col]], na.rm = T),
                      na = sum(is.na(.data[[col]])),
                      prcnt_na = na / nrow(df)) %>%
            distinct() %>%
            mutate(across(where(is.numeric), ~round(.x, digits = 2)))
        
    } else if(is.factor(df[[col]])) {
        
        df %>%
            group_by(.data[[col]]) %>%
            summarise(n = n()) %>%
            arrange(desc(n)) %>%
            ungroup() %>%
            mutate(prcnt = n / sum(n)) %>%
            pivot_longer(all_of(col),
                         values_transform = list(value = as.character))
        
    } else if(is.logical(df[[col]])) {
        
        df %>%
            transmute(col = col,
                      mean = mean(.data[[col]]),
                      true = sum(.data[[col]]),
                      false = sum(!.data[[col]]),
                      missing = sum(is.na(.data[[col]])),
                      prcnt_missing = missing / nrow(df)) %>%
            distinct()
        
    } else {
        df %>%
            transmute(col = col,
                      missing = sum(is.na(.data[[col]])),
                      prcnt_missing = missing / nrow(df)) %>%
            distinct()
    }
}

GetSummary <- function(df) {

    col_types <- unlist(map(df, function(col) {class(col)[1]}))
    cols <- colnames(df)
    names(cols) <- col_types
    
    summary <- map(cols, GetSummaryCol, df = df)
    
    numeric <- bind_rows(summary[names(summary) == "numeric"])
    factor <- bind_rows(summary[names(summary) == "factor"])
    logical <- bind_rows(summary[names(summary) == "logical"])
    other <- bind_rows(summary[
        !(names(summary) %in% c("numeric", "factor", "logical"))])
    
    summaries <- list("numeric" = numeric,
                      "factor" = factor,
                      "logical" = logical,
                      "other" = other)
    
    if(length(summaries[["numeric"]]) == 0) {
        summaries[["numeric"]] <- NULL
    } else if(length(summaries[["factor"]]) == 0) {
        summaries[["factor"]] <- NULL
    } else if(length(summaries[["logical"]]) == 0) {
        summaries[["logical"]] <- NULL
    } else if(length(summaries[["other"]]) == 0) {
        summaries[["other"]] <- NULL
    }
    
    return(summaries)
}
