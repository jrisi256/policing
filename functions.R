library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

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

# Summarize a column
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

# Given a data frame, summarise each column
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
