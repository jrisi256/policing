library(here)
source(here("functions.R"))

# Read in shift assignments (independent variables)
shift_assignments <- my_read_csv(here("create-outcomes",
                                      "input",
                                      "officer_assignments_ba.csv"))

# Read in outcomes (dependent variables)
outcomes <- my_read_csv(here("create-outcomes",
                             "output",
                             "outcomes_raw_ba_max.csv"),
                        injured = T)

groups_by_date <-
    shift_assignments %>%
    count(beat_assigned, weekday, shift, month, date, officer_race) %>%
    pivot_wider(names_from = officer_race, values_from = n, values_fill = 0) %>%
    mutate(n = officer_white + officer_black + officer_hisp,
           bw_comparisons_samedate = officer_black * officer_white,
           hw_comparisons_samedate = officer_hisp * officer_white)

groups_by_mdsb <-
    groups_by_date %>%
    group_by(beat_assigned, weekday, shift, month) %>%
    summarise(across(officer_white:hw_comparisons_samedate, sum)) %>%
    mutate(nr_races =
               as.numeric(officer_white > 0) +
               as.numeric(officer_black > 0) +
               as.numeric(officer_hisp > 0),
           bw_comparisons = officer_white * officer_black,
           hw_comparisons = officer_white * officer_hisp)

feasible_groups <-
    groups_by_mdsb %>%
    filter(nr_races > 1) %>%
    select(beat_assigned, weekday, shift, month)

# keep only shift assignments with more than one racial group
# those with only one racial group will contribute 0 to the estimate
shift_assignments_feasible <-
    inner_join(shift_assignments, feasible_groups) %>%
    inner_join(outcomes, by = "shift_id") %>%
    mutate(officer_race = fct_relevel(officer_race,
                                      "officer_white",
                                      "officer_black",
                                      "officer_hisp"),
           month = as.factor(month)) %>%
    select(officer_id, month, beat_assigned, weekday, shift, officer_race,
           stops_n, arrests_n, force_n, months_from_start, months_from_start_sq,
           shift_id)
setDT(shift_assignments_feasible)

############################################## Ba et al. function for demeaning
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

####################################################### Constants for Ba et al.
fe <- c("month", "shift", "weekday", "beat_assigned")
ys <- c("stops_n", "arrests_n", "force_n")
d <- "officer_race"

###################################################### Ba demeaning process
stops_ba <- demean.by.group(shift_assignments_feasible,
                            id = 'officer_id',
                            fe = fe,
                            ds = d,
                            ys = c(ys[grep('stops', ys)],
                                   'months_from_start',
                                   'months_from_start_sq')) %>%
    select(stops_n, officer_black, officer_hisp)

arrests_ba <- demean.by.group(shift_assignments_feasible,
                                  id = 'officer_id',
                                  fe = fe,
                                  ds = d,
                                  ys = c(ys[grep('arrests', ys)],
                                         'months_from_start',
                                         'months_from_start_sq')) %>%
    select(arrests_n, officer_black, officer_hisp)

force_ba <- demean.by.group(shift_assignments_feasible,
                                id = 'officer_id',
                                fe = fe,
                                ds = d,
                                ys = c(ys[grep('force', ys)],
                                       'months_from_start',
                                       'months_from_start_sq')) %>%
    select(force_n, officer_black, officer_hisp)

############################################ Risi demeaning process
stops_risi <-
    shift_assignments_feasible %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = officer_race,
                values_from = dummy,
                values_fill = 0) %>%
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
    ungroup() %>%
    select(stops_n_demean, officer_black_demean, officer_hisp_demean, shift_id) %>%
    rename(stops_risi = stops_n_demean,
           officer_black_stop_risi = officer_black_demean,
           officer_hisp_stop_risi = officer_hisp_demean)

arrests_risi <-
    shift_assignments_feasible %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = officer_race,
                values_from = dummy,
                values_fill = 0) %>%
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
    ungroup() %>%
    select(arrests_n_demean, officer_black_demean, officer_hisp_demean, shift_id) %>%
    rename(arrest_risi = arrests_n_demean,
           officer_black_arrest_risi = officer_black_demean,
           officer_hisp_arrest_risi = officer_hisp_demean)

force_risi <-
    shift_assignments_feasible %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = officer_race,
                values_from = dummy,
                values_fill = 0) %>%
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
    ungroup() %>%
    select(force_n_demean, officer_black_demean, officer_hisp_demean, shift_id) %>%
    rename(force_risi = force_n_demean,
           officer_black_force_risi = officer_black_demean,
           officer_hisp_force_risi = officer_hisp_demean)


outcomes_risi <- reduce(list(stops_risi, arrests_risi, force_risi),
                        full_join, by = "shift_id")

write_csv(outcomes_risi,
          here("create-outcomes",
               "output",
               "outcomes_final_risi_replicate_ba_max.csv"))

write_csv(stops_ba,
          here("create-outcomes",
               "output",
               "outcomes_final_ba_max_stops.csv"))

write_csv(arrests_ba,
          here("create-outcomes",
               "output",
               "outcomes_final_ba_max_arrests.csv"))

write_csv(force_ba,
          here("create-outcomes",
               "output",
               "outcomes_final_ba_max_force.csv"))
