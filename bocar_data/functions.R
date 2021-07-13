###############
## functions ##
###############

`%.%` <- paste0

## uppercase first char in a string
capitalize <- function(x){
    paste0(toupper(substr(x, 1, 1)),
           tolower(substr(x, 2, nchar(x)))
    )
}

## convert to decimal years
yearmonth <- function(x){
    year(x) + month(x) / 12
}

## get closest positive-definite matrix
##   by truncating negative eigenvalues and reconstructing
closest.pd <- function(x){
    x.eigen <- eigen(x)
    x.eigen$vectors %*% diag(pmax(x.eigen$values, 0)) %*% solve(x.eigen$vectors)
}

## convert one-sided to two-sided p-values
pval.1side.to.2side <- function(p){ 2 * pmin(p, 1 - p) }

## convert p-values to stars
pval.to.stars <- function(p){
    as.character(cut(
        p,
        breaks = c(-Inf, .001, .01, .05, Inf),
        labels = c('$^{***}$', '$^{**}$', '$^{*}$', '')
        ## labels = c('***', '**', '*', '')
    ))
}

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

## from script 4, 9
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

## script 5, 10
differences.in.gender.means <- function(summary, fe, combined = TRUE){
    ds <- c('FEMALE', 'MALE')
    ys <- unique(gsub(
        '(FEMALE|MALE)_(.*)_(mean|sd)',
        '\\2',
        colnames(summary)[!colnames(summary) %in% c(fe, ds %.% '_n')]
    ))
    ## compute various weighted differences for each outcome and treatment status
    out <- ddply(
        expand.grid(d = 'FEMALE',
                    y = ys
        ),
        c('d', 'y'),
        function(x){
            ## extract relevant stats for each treatment status x group
            y.treat <- summary[[paste(x$d, x$y, 'mean', sep = '_')]]
            y.ctrl <- summary[[paste('MALE', x$y, 'mean', sep = '_')]]
            n.treat <- summary[[paste(x$d, 'n', sep = '_')]]
            n.ctrl <- summary[['MALE_n']]
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
    out$treatment <- 'officer_' %.% tolower(out$treatment) %.% '_vs_male'
    return(out)
}

## script 6, 11
differences.in.language.means <- function(summary, fe, combined = TRUE){
    ds <- 'officer_' %.% c('hisp_spanish', 'hisp_nospanish')
    ys <- unique(gsub(
        'officer_(hisp_spanish|hisp_nospanish)_(.*)_(mean|sd)',
        '\\2',
        colnames(summary)[!colnames(summary) %in% c(fe, ds %.% '_n')]
    ))
    ## compute various weighted differences for each outcome and treatment status
    out <- ddply(
        expand.grid(d = 'officer_hisp_spanish',
                    y = ys
        ),
        c('d', 'y'),
        function(x){
            ## extract relevant stats for each treatment status x group
            y.treat <- summary[[paste(x$d, x$y, 'mean', sep = '_')]]
            y.ctrl <- summary[[paste('officer_hisp_nospanish', x$y, 'mean', sep = '_')]]
            n.treat <- summary[[paste(x$d, 'n', sep = '_')]]
            n.ctrl <- summary[['officer_hisp_nospanish_n']]
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
    out$treatment <- out$treatment %.% '_vs_officer_hisp_nospanish'
    return(out)
}

## from script 4, 5, 6, 7, 9, 10, 11
demean.by.group <- function(data, fe, d, ys){
    data <- na.omit(data[, c(ys, d, fe), with = FALSE])
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
    ## demean d by group: construct model matrix
    d.demean <- model.matrix(~ 0 + get(d), data)
    colnames(d.demean) <- gsub('get\\(.*\\)', '', colnames(d.demean))
    ds <- colnames(d.demean)
    ## demean d by group: append grouping vars
    d.demean <- data.table(d.demean,
                           data[, fe, with = FALSE]
    )
    ## demean d by group: loop over different dummy ds
    for (j in ds){
        d.demean[
            ,
            (j) := get(j) - mean(get(j)),
            by = fe
        ]
    }
    ## check:
    if (!identical(ys.demean[, fe, with = FALSE], d.demean[, fe, with = FALSE])){
        stop('failed to merge demeaned y and d')
    }
    ## check:
    if (!identical(ys.demean[, fe, with = FALSE], data[, fe, with = FALSE])){
        stop('failed to merge demeaned y and original data')
    }
    return(cbind(
        ys.demean[, -(fe), with = FALSE],
        d.demean,
        data[, d, with = FALSE]
    ))
}

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
