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
