library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(GGally)
library(stringr)
library(ggplot2)
library(forcats)
library(lubridate)
library(data.table)

# Find if any shift assignments with missing start/end times matched to an outcome
find_missing <- function(df, id_var) {
    
    df %>%
        filter((is.na(start_time) | is.na(end_time)) & !is.na({{ id_var }}))
}

# Find if any outcomes matched to multiple shift assignments
find_dupes <- function(df, group_var) {
    
    df %>%
        group_by({{ group_var }}) %>%
        filter(!is.na({{ group_var }}) & n() > 1) %>%
        ungroup()
}

# Given a merged data set i.e. an outcome (e.g. stops) with shift assignments,
# return a data frame with summary statistics
PrettyPrint <- function(outcome_merged, outcome, shifts, missings, dupes, id_col) {
    
    df <-
        tibble(nrow = nrow(outcome_merged),
               nr_outcome = length(unique(na.omit(outcome_merged[[id_col]]))),
               prcnt_outcome = nr_outcome / nrow(outcome) * 100,
               nr_shift = length(unique(outcome_merged$shift_id)),
               prcnt_shift = nr_shift / nrow(shifts) * 100,
               nr_missing_shifts = length(unique(missings$shift_id)),
               prcnt_missing_shifts = nr_missing_shifts / nrow(shifts) * 100,
               nr_missing_outcomes = length(unique(missings[[id_col]])),
               prcnt_missing_outcomes = nr_missing_outcomes / nrow(outcome) * 100,
               nr_dupe_outcomes = length(unique(dupes[[id_col]])),
               prcnt_dupe_outcomes = nr_dupe_outcomes / nrow(outcome) * 100,
               nr_dupe_shifts = length(unique(dupes$shift_id)),
               prcnt_dupe_shifts = nr_dupe_shifts / nrow(shifts) * 100,
               nr_dupes = nrow(dupes),
               prcnt_dupes = nr_dupes / nrow * 100)
}

# Merges an outcome to shift assignments using the process used in Ba et al. 2021
# Returns the resulting joined data frame
ba_merge <- function(df1, df2, id_col) {
    
    id_col <- enquo(id_col)
    
    # sort for merging
    setkey(df1, officer_id, date)
    setkey(df2, officer_id, date)
    
    ## step 1: match officer_id, match stop date to date of shift start
    merged <- df2[df1]
    merged[, month := i.month]
    merged <- merged[, !"i.month", with = F]
    
    ## drop if not during shift (keep those shifts with no stops)
    merged <-
        merged[
            is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),]
    
    ## step 2: match officer_id, deal with stops past midnight on overnight shifts
    ####  Find overnight shifts, fix start and end time, increment date by one day
    nextday <- df1[end_time > 24]
    nextday[, new_start_time := 0]
    nextday[, new_end_time := end_time - 24]
    nextday[, date_next := date]
    day(nextday$date_next) <- day(nextday$date_next) + 1
    
    ## Match officer_id and stop date to the new start date of the overnight shift
    setkey(nextday, officer_id, date_next)
    merged_nextday <-
        df2[nextday, , on = c(officer_id = 'officer_id', date = 'date_next')]
    
    ## drop if not during shift (keep those shifts with no stops)
    merged_nextday <-
        merged_nextday[
            is.na(hour) |
                between(hour, floor(new_start_time), ceiling(new_end_time)),]
    
    ## Revert back to the normal date
    drop_cols <- c("i.date", "new_end_time", "new_start_time", "i.month")
    merged_nextday[, `:=`(date = i.date, month = i.month)]
    merged_nextday <- merged_nextday[, !drop_cols, with = F]
    
    ## merge same-day stops with next-day stops on overnight shifts
    rbind(merged, merged_nextday) %>%
        distinct() %>%
        group_by(shift_id) %>%
        filter((n() > 1 & !is.na(!!id_col)) | n() == 1) %>%
        ungroup()
}

# Takes in a file path and returns a data frame with the correct column types.
my_read_csv <- function(file_path) {
    
    df <- read_csv(file_path)
    
    # Factors, characters, logical. Everything not mentioned uses the default.
    f <- "race|gender|district|rank|weekday|unit|shift|type|code"
    c <- "id|lat|lon"
    l <- "spanish|injured|first"
    
    df %>%
        mutate(across(matches(f), function(col) {as.factor(col)}),
               across(matches(c), function(col) {as.character(col)}),
               across(matches(l), function(col) {as.logical(col)}))
}

# Summarize a column. Returns a data frame.
GetSummaryCol <- function(df, col) {
    
    if(is.numeric(df[[col]])) {
        
        q <- quantile(df[[col]], na.rm = T)
        
        # Numeric columns
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
        
        # Factors
        df %>%
            group_by(.data[[col]]) %>%
            summarise(n = n()) %>%
            arrange(desc(n)) %>%
            ungroup() %>%
            mutate(prcnt = n / sum(n)) %>%
            pivot_longer(all_of(col),
                         values_transform = list(value = as.character))
        
    } else if(is.logical(df[[col]])) {
        
        # Logical
        df %>%
            transmute(col = col,
                      mean = mean(.data[[col]]),
                      true = sum(.data[[col]]),
                      false = sum(!.data[[col]]),
                      missing = sum(is.na(.data[[col]])),
                      prcnt_missing = missing / nrow(df)) %>%
            distinct()
        
    } else {
        
        # Everything not a factor, numeric, or a logical
        df %>%
            transmute(col = col,
                      missing = sum(is.na(.data[[col]])),
                      nr_unique = length(unique(.data[[col]])),
                      prcnt_missing = missing / nrow(df)) %>%
            distinct()
    }
}

# Given a data frame, summarise each column. Returns a list of data frames.
GetSummary <- function(df) {

    # Get the column type for each column.
    # Some columns return more than one type which is why I need to subset.
    # E.g. POSIXct
    col_types <- unlist(map(df, function(col) {class(col)[1]}))
    cols <- colnames(df)
    names(cols) <- col_types
    
    # Returns a data frame for each column.
    summary <- map(cols, GetSummaryCol, df = df)
    
    # Bind each data frame together of the same variable type.
    numeric <- bind_rows(summary[names(summary) == "numeric"])
    factor <- bind_rows(summary[names(summary) == "factor"])
    logical <- bind_rows(summary[names(summary) == "logical"])
    other <- bind_rows(summary[
        !(names(summary) %in% c("numeric", "factor", "logical"))])
    
    summaries <- list("numeric" = numeric,
                      "factor" = factor,
                      "logical" = logical,
                      "other" = other)
    
    # Delete any entries with no data frames in them.
    # Happens when the data frame does not have any columns of a specific type.
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
