library(plyr)
library(data.table)
library(here)
library(dplyr)
library(readr)
library(tidyr)

my_read_csv <- function(file_path, injured = F) {
    
    # read in injured as a numeric to avoid coercion and loss of information
    if(injured)
    {
        df <- read_csv(file_path,
                       col_types = cols(force_injured_true = "d",
                                        force_injured_false = "d"))
    }
    else
    {
        df <- read_csv(file_path)
    }
    
    # Factors, characters, logical, and numeric. Those not mentioned use default.
    f <- "race|gender|district|rank|weekday|unit|shift|type|code"
    c <- "id|lat|lon"
    l <- "spanish|first"
    
    df %>%
        mutate(across(matches(f), function(col) {as.factor(col)}),
               across(matches(c), function(col) {as.character(col)}),
               across(matches(l), function(col) {as.logical(col)}))
}

outcomes <- my_read_csv(here::here("create-outcomes",
                                   "output",
                                   "outcomes_feasible_ba_max.csv")) %>%
    select(officer_id, month, beat_assigned, weekday, shift, officer_race,
           stops_n, arrests_n, force_n, months_from_start, months_from_start_sq,
           shift_id)

setDT(outcomes)

means.by.group <- function(data, fe, d, ys){
    ## tabulate event means/sds within group
    data <- na.omit(data[, c(fe, d, ys), with = FALSE])
    data.by.group <- data[
        ,
        c(
            n = .N,
            {
                out <- lapply(.SD, mean)
                names(out) <- names(out) %.% '_mean'
                out
            }
        ),
        by = c(fe, d),
        .SDcols = ys
    ]
    colnames(data.by.group)[colnames(data.by.group) == d] <- 'treatment'
    ## reshape so that each group is a row (different treatments on cols)
    data.by.group <- melt(data.by.group,
                          id.vars = c(fe, 'treatment')
    )
    data.by.group <- dcast(
        data.by.group,
        as.formula(sprintf(
            '%s ~ treatment + variable',
            paste(fe, collapse = ' + ')
        )),
        value.var = 'value',
        fill = NA
    )
    return(data.by.group)
}

fe <- c("month", "shift", "weekday", "beat_assigned")
ys <- c("stops_n", "arrests_n", "force_n")
d <- "officer_race"
`%.%` <- paste0

assignments.summary <- means.by.group(outcomes, fe = fe, d = d, ys = ys)

differences.in.race.means <- function(summary, fe, combined = TRUE){
    ds <- 'officer_' %.% c('black', 'hisp', 'white')
    ys <- unique(gsub(
        'officer_(black|hisp|white|aapi|native)_(.*)_(mean|sd)',
        '\\2',
        colnames(summary)[!colnames(summary) %in% c(fe, ds %.% '_n')]
    ))
    ## compute various weighted differences for each outcome and treatment status
    out <- ddply(
        expand.grid(d = 'officer_' %.% c('black', 'hisp'),
                    y = ys
        ),
        c('d', 'y'),
        function(x){
            ## extract relevant stats for each treatment status x group
            y.treat <- summary[[paste(x$d, x$y, 'mean', sep = '_')]]
            y.ctrl <- summary[[paste('officer_white', x$y, 'mean', sep = '_')]]
            n.treat <- summary[[paste(x$d, 'n', sep = '_')]]
            n.ctrl <- summary[['officer_white_n']]
            if (combined){
                ## compute differences and return
                c(diff.simple = mean(y.treat - y.ctrl, na.rm = TRUE),
                  diff.countweight = weighted.mean(
                      y.treat - y.ctrl,
                      n.treat + n.ctrl,
                      na.rm = TRUE
                  )
                )
            } else {
                data.frame(summary[, fe, with = FALSE],
                           y.treat,
                           y.ctrl,
                           n.treat,
                           n.ctrl,
                           diff = y.treat - y.ctrl,
                           n = n.treat + n.ctrl
                )
            }
        })
    colnames(out)[colnames(out) == 'd'] <- 'treatment'
    colnames(out)[colnames(out) == 'y'] <- 'outcome'
    out$treatment <- out$treatment %.% '_vs_white'
    return(out)
}

results.diffmeans <- differences.in.race.means(assignments.summary, fe = fe)
results.diffmeans <- data.table(results.diffmeans)
setkey(results.diffmeans, treatment, outcome)

# This is what the function is doing
mean(assignments.summary$officer_black_arrests_n_mean - assignments.summary$officer_white_arrests_n_mean, na.rm = T)

demean.by.group <- function(data, id, fe, ds, ys){
    data <- na.omit(data[, c(ys, ds, fe, id), with = FALSE])
    ## demean y by group
    ys.demean <- data[, c(ys, fe), with = FALSE]
    ## cast to double then demean
    for (y in ys){
        ys.demean[[y]] <- as.numeric(ys.demean[[y]])
        ys.demean[
            ,
            (y) := get(y) - mean(get(y)),
            by = fe
        ]
    }
    ## loop over treatment variables
    d.demean.list <- list()
    for (k in seq_along(ds)){
        d <- ds[k]
        ## for each one, demean d by group: construct model matrix
        d.demean <- model.matrix(~ 0 + get(d), data)
        colnames(d.demean) <- gsub('get\\(.*\\)', '', colnames(d.demean))
        d.levels <- colnames(d.demean)
        ## demean d by group: append grouping vars
        d.demean <- data.table(d.demean,
                               data[, fe, with = FALSE]
        )
        ## demean d by group: loop over different dummy levels
        for (j in d.levels){
            d.demean[
                ,
                (j) := get(j) - mean(get(j)),
                by = fe
            ]
        }
        d.demean.list[[k]] <- d.demean
    }
    ## merge treatment variables
    d.demean <- d.demean.list[[1]]
    for (k in seq_along(ds)[-1]){
        ## check:
        if (!identical(d.demean[, fe, with = FALSE],
                       d.demean.list[[k]][, fe, with = FALSE]
        )
        ){
            stop('failed to merge demeaned ds')
        } else {
            d.demean <- cbind(d.demean.list[[k]][, -fe, with = FALSE], d.demean)
        }
    }
    ## check: merging treatments with outcomes
    if (!identical(ys.demean[, fe, with = FALSE], d.demean[, fe, with = FALSE])){
        stop('failed to merge demeaned y and d')
    }
    ## check: merge output with original data
    if (!identical(ys.demean[, fe, with = FALSE], data[, fe, with = FALSE])){
        stop('failed to merge demeaned y and original data')
    }
    return(cbind(
        data[, id, with = FALSE],
        ys.demean[, -(fe), with = FALSE],
        d.demean,
        data[, ds, with = FALSE]
    ))
}

stops.demean <- demean.by.group(
    outcomes,
    id = 'officer_id',
    fe = fe,
    ds = d,
    ys = c(
        ys[grep('stops', ys)],
        'months_from_start',
        'months_from_start_sq'
    )
)

arrests.demean <- demean.by.group(
    outcomes,
    id = 'officer_id',
    fe = fe,
    ds = d,
    ys = c(
        ys[grep('arrests', ys)],
        'months_from_start',
        'months_from_start_sq'
    )
)

force.demean <- demean.by.group(
    outcomes,
    id = 'officer_id',
    fe = fe,
    ds = d,
    ys = c(
        ys[grep('force', ys)],
        'months_from_start',
        'months_from_start_sq'
    )
)

a2 <-
    filter(outcomes,
           shift == 3,
           weekday == "Sun",
           beat_assigned == "2544",
           month == "2012-06-01") %>%
    mutate(across(c(stops_n, months_from_start, months_from_start_sq),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(stops_n_demean = stops_n - stops_n_mean,
           months_from_start_demean = months_from_start - months_from_start_mean,
           months_from_start_sq_demean = months_from_start_sq - months_from_start_sq_mean,
           dummy = 1) %>%
    pivot_wider(names_from = officer_race, values_from = dummy, values_fill = 0) %>%
    mutate(officer_white_demean = officer_white - mean(officer_white),
           officer_black_demean = officer_black - mean(officer_black))

b2 <- filter(stops.demean, shift == 3, weekday == "Sun", beat_assigned == "2544", month == "2012-06-01")

test <-
    outcomes %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = officer_race,
                values_from = dummy,
                values_fill = 0)
    
test_stop <-
    test %>%
    filter(!is.na(stops_n)) %>%
    group_by(shift, weekday, beat_assigned, month) %>%
    mutate(across(c(stops_n, officer_white, officer_black,
                    officer_hisp),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(stops_n_demean = stops_n - stops_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean) %>%
    ungroup()

test_arrest <-
    test %>%
    filter(!is.na(arrests_n)) %>%
    group_by(shift, weekday, beat_assigned, month) %>%
    mutate(across(c(arrests_n, officer_white, officer_black,
                    officer_hisp),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(arrests_n_demean = arrests_n - arrests_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean) %>%
    ungroup()

test_force <-
    test %>%
    filter(!is.na(force_n)) %>%
    group_by(shift, weekday, beat_assigned, month) %>%
    mutate(across(c(force_n, officer_white, officer_black,
                    officer_hisp),
                  mean,
                  .names = "{.col}_mean")) %>%
    mutate(force_n_demean = force_n - force_n_mean,
           officer_white_demean = officer_white - officer_white_mean,
           officer_black_demean = officer_black - officer_black_mean,
           officer_hisp_demean = officer_hisp - officer_hisp_mean) %>%
    ungroup()
    

 c2 <- filter(test, shift == 3, weekday == "Sun", beat_assigned == "2544", month == "2012-06-01")

stops_race <- lm(stops_n_demean ~ officer_black_demean + officer_hisp_demean, data = test_stop)
stops_race_ba <- lm(stops_n ~ officer_black + officer_hisp, data = stops.demean)

arrests_race <- lm(arrests_n_demean ~ officer_black_demean + officer_hisp_demean, data = test_arrest)
arrests_race_ba <- lm(arrests_n ~ officer_black + officer_hisp, data = arrests.demean)

force_race <- lm(force_n_demean ~ officer_black_demean + officer_hisp_demean, data = test_force)
force_race_ba <- lm(force_n ~ officer_black + officer_hisp, data = force.demean)

a <-
    test %>%
    filter(!is.na(stops_n)) %>%
    arrange(officer_white_demean)

stops.demean <-
    stops.demean %>%
    arrange(officer_white)

compare <- bind_cols(risi = a$officer_white_demean,
                     ba = stops.demean$officer_white) %>%
    mutate(diff = risi - ba)
