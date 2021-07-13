options(stringsAsFactors = FALSE)

## data manipulation
library(here)
library(plyr)
library(data.table)

## math
library(speedglm)

## parallel
library(foreach)
library(doParallel)
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

## output
library(xtable)
library(ggplot2)

source(here("bocar_data", "functions.R"))



####################################################
## load preprocessed shift assignments & behavior ##
####################################################

assignments <- fread(here("bocar_data", 'assignments_with_outcomes.csv'))



##########################################
## average behavior of officers by race ##
##########################################

d <- 'officer_race'
ys <- colnames(assignments)[
    grep('^(stops|arrests|force)_n', colnames(assignments))
]
ys <- ys[-grep('civilian_(native|aapi|other|unknownrace)', ys)]

## compute summary statistics (pooled and by officer race)
assignments.summary <- data.table(
    outcome = rev(ys),
    mean.pooled = as.numeric(
        assignments[,
                    lapply(.SD, mean, na.rm = TRUE),
                    .SDcols = rev(ys)
        ]
    ),
    mean.black = as.numeric(
        assignments[officer_race == 'officer_black',
                    lapply(.SD, mean, na.rm = TRUE),
                    by = d,
                    .SDcols = rev(ys)
        ][, -'officer_race']
    ),
    mean.hisp = as.numeric(
        assignments[officer_race == 'officer_hisp',
                    lapply(.SD, mean, na.rm = TRUE),
                    by = d,
                    .SDcols = rev(ys)
        ][, -'officer_race']
    ),
    mean.white = as.numeric(
        assignments[officer_race == 'officer_white',
                    lapply(.SD, mean, na.rm = TRUE),
                    by = d,
                    .SDcols = rev(ys)
        ][, -'officer_race']
    )
)

outcomes.to.labels <- c(
    'civilian_black'  = 'Civ. race: Black',
    'civilian_hisp'   = 'Civ. race: Hispanic',
    'civilian_white'  = 'Civ. race: White',
    'loitering'       = 'Reason: Loitering',
    'suspicious'      = 'Reason: Suspicious',
    'mcc'             = 'Reason: Code',
    'drug'            = 'Reason: Drug',
    'traffic'         = 'Reason: Traffic',
    'trf'             = 'Reason: Traffic',
    'wrt'             = 'Reason: Warrant',
    'property'        = 'Reason: Property',
    'violent'         = 'Reason: Violent',
    'arrestsearch'    = 'Reason: Arrest/search',
    'force'           = 'Reason: Force',
    'other'           = 'Reason: Other',
    'injury'          = 'Result: Injury',
    'total'           = 'Total'
)
assignments.summary[,
                    outcome.type := factor(
                        gsub('(stops|arrests|force).*',
                             '\\1',
                             outcome
                        ),
                        levels = c('stops', 'arrests', 'force'),
                        ordered = TRUE
                    )
]
assignments.summary[,
                    outcome.label := factor(
                        outcomes.to.labels[
                            gsub('(stops|arrests|force)_n_',
                                 '',
                                 tolower(outcome)
                            )
                        ],
                        levels = unique(outcomes.to.labels),
                        ordered = TRUE
                    )
]

## rescale outcomes
assignments.summary[, outcome.scale := c(stops = 100,
                                         arrests = 100,
                                         force = 100
)[outcome.type]
]
js <- colnames(assignments.summary)[
    colnames(assignments.summary) %like% 'mean'
]
for (j in js){
    assignments.summary[[j]] <-
        assignments.summary[[j]] * assignments.summary[['outcome.scale']]
}



## sort and output
setkey(assignments.summary, outcome.type, outcome.label)
fwrite(assignments.summary[,
                           c('outcome', js, 'outcome.scale'),
                           with = FALSE
],
here("bocar_data", 'outcomes_mean_race_scaled.csv')
)
sink(here("bocar_data", 'outcomes_mean_race_scaled.tex'))
for (outcome.type in c('stops', 'arrests', 'force')){
    print(
        xtable(
            assignments.summary[outcome.type,
                                c('outcome.label', js),
                                with = FALSE
            ],
            caption = sprintf('%s per %s shifts',
                              capitalize(outcome.type),
                              assignments.summary[outcome.type, outcome.scale[1]]
            )
        ),
        digits = 2,
        include.rownames = FALSE
    )
    cat('\n')
}
sink()



###########################################################################
## analyze behavior of officers by race, relative to others in same mdsb ##
###########################################################################

fe <- c('beat_assigned', 'weekday', 'shift', 'month')
setkeyv(assignments, c(fe, d))
boot.start <- 0
boot.end <- 5000

## use most frequent home unit of assigned officer as proxy
##   for the administrative parent unit of that beat
beat.to.unit <- assignments[, .N, by = c('beat_assigned', 'unit')]
beat.to.unit <-
    beat.to.unit[, .(unit = unit[which.max(N)]), by = 'beat_assigned']

## summary stats on mdsb, breaking out unique days separately to assess
##   how often comparisons are made between officers working on the same date
assignments.groups.by.date <-
    assignments[, .(.N,
                    N.black = sum(officer_race == 'officer_black'),
                    N.hisp = sum(officer_race == 'officer_hisp'),
                    N.white = sum(officer_race == 'officer_white')
    ),
    by = c('date', fe)
    ]
assignments.groups.by.date[, comparisons.bw.samedate := N.black * N.white]
assignments.groups.by.date[, comparisons.hw.samedate := N.hisp * N.white]

## aggregate to mdsb level, collapsing unique days in a month
assignments.groups <- assignments.groups.by.date[
    ,
    lapply(.SD, sum),
    by = fe,
    .SDcols = c('N.' %.% c('black', 'hisp', 'white'),
                'comparisons.' %.% c('bw', 'hw') %.% '.samedate'
    )
]
assignments.groups[, N := N.black + N.hisp + N.white]
assignments.groups[,
                   N.races :=
                       (N.black > 0) + (N.hisp > 0) + (N.white > 0)
]
assignments.groups[, comparisons.bw := N.black * N.white]
assignments.groups[, comparisons.hw := N.hisp * N.white]

## examine feasible mdsbs by home unit
## merge in home unit for each beat
assignments.groups <- beat.to.unit[assignments.groups, on = 'beat_assigned']
## merge in home district demographics
districts.majority <- fread('../results/summary/district_majority_race.csv')
colnames(districts.majority)[2] <- 'majority'
assignments.groups <-
    districts.majority[assignments.groups, on = c(district = 'unit')]
## proportion of feasible mdsb by district majority race
feasible.summary <- na.omit(assignments.groups[
    ,
    .(prop.feasible.shifts = sum(N[N.races > 1]) / sum(N)),
    by = 'majority'
])
setkey(feasible.summary, majority)
fwrite(feasible.summary, '../results/counts/mdsb_summary_race_feasibility.csv')

## examine how feasible and infeasible mdsb differ
##   - size (number of officer-shifts)
##   - alphabetical suffixes (typically subdivided mdsb)
assignments.groups[, suffix := substr(beat_assigned,
                                      nchar(beat_assigned),
                                      nchar(beat_assigned)
)
]
assignments.groups[, suffix_alpha := suffix %chin% LETTERS]
sink('../results/counts/mdsb_summary_race_feasibility_explanations.txt')
cat('proportion of infeasible (x) and feasible (y) mdsb with a-d suffixes\n')
print(
    t.test(assignments.groups[N.races == 1, suffix %chin% LETTERS[1:4]],
           assignments.groups[N.races > 1, suffix %chin% LETTERS[1:4]]
    )
)
cat(rep('-', 80), sep = '')
cat('\n\naverage officer-shifts in infeasible (x) and feasible (y) mdsb\n\n')
print(
    t.test(assignments.groups[N.races == 1, N],
           assignments.groups[N.races > 1, N]
    )
)
sink()

## Make plot of # officers per feasible mdsb
# get district racial majorities
assignments.groups.bvw <- assignments.groups[!is.na(majority)]
assignments.groups.bvw[, BvW := N.black - N.white]
assignments.groups.bvw[, feasible := ifelse(N.black > 1 & N.white > 1,
                                            'Black-White\ncomparison\nis feasible',
                                            'Black-White\ncomparison\nis infeasible'
)
]
assignments.groups.bvw[, majority.label := 'District majority:\n' %.% majority]

pdf('../results/counts/mdsb_feasible_bvw_dist.pdf', 12, 6)
ggplot(
    assignments.groups.bvw,
    aes(x = BvW,
        group = majority,
        color = majority,
        fill = majority
    )
) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_histogram(aes(y = stat(density)), fill = NA, binwidth = 1) +
    theme(text = element_text(family = 'Helvetica',
                              size = 14,
                              vjust = 1
    )) +
    facet_grid(feasible ~ majority.label) +
    ## theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    ##       panel.background = element_blank()) +
    ## theme(plot.margin = grid::unit(c(0,0,4,0), 'mm'))  +
    theme_light(base_size = 20) +
    theme(legend.position = 'bottom') +
    xlab('\nDifference in number of Black and White officers assigned to MDSB') +
    ylab('Proportion\n') +
    scale_color_manual(name = 'Majority Race',
                       values = c(Black = '#A31F34',
                                  None = '#4F6F9C',
                                  Hispanic = '#CB963F',
                                  White = 'black'
                       ),
                       guide = FALSE
    ) +
    scale_fill_manual(name = 'Majority Race',
                      values = c(Black = '#A31F34',
                                 None = '#4F6F9C',
                                 Hispanic = '#CB963F',
                                 White = 'black'
                      ),
                      guide = FALSE
    ) +
    xlim(-10, 10)
dev.off()
rm(assignments.groups.bvw)

## subset to assignments in feasible mdsb (containing both groups
##   (those with only 1 group will contribute zero to estimate)
assignments <- assignments[
    assignments.groups[N.races > 1, .(beat_assigned, weekday, shift, month)],
]

## compare with overall numbers
assignments.overview <- fread('../results/summary/dataset_overview.csv')

## summary stats on mdsb
sink('../results/counts/mdsb_summary_race.txt')
cat('\nnumber of unique mdsb:',
    nrow(assignments.groups)
)
cat('\nnumber of mdsb with >1 officer:',
    nrow(assignments.groups[N > 1,])
)
cat('\nnumber of mdsb with >1 officer race (racial comparison is feasible):',
    nrow(assignments.groups[N.races > 1])
)
cat('\n', rep('-', 80), sep = '')
cat('\nproportion of mdsbs with racial comparisons:',
    nrow(assignments.groups[N.races > 1]) / nrow(assignments.groups)
)
cat('\n', rep('-', 80), sep = '')
cat('\nproportion of shifts in mdsbs with racial comparisons:',
    assignments.groups[N.races > 1, sum(N)] / assignments.groups[, sum(N)]
)
cat('\nproportion of black-officer shifts in mdsbs with racial comparisons:',
    assignments.groups[N.races > 1, sum(N.black)] /
        assignments.groups[, sum(N.black)]
)
cat('\nproportion of hisp-officer shifts in mdsbs with racial comparisons:',
    assignments.groups[N.races > 1, sum(N.hisp)] /
        assignments.groups[, sum(N.hisp)]
)
cat('\nproportion of black-officer shifts in mdsbs with racial comparisons:',
    assignments.groups[N.races > 1, sum(N.white)] /
        assignments.groups[, sum(N.white)]
)
cat('\n', rep('-', 80), sep = '')
cat('\nproportion of officers with feasible racial comparison:',
    uniqueN(assignments$officer_id) / (
        assignments.overview[Behavior == 'Officers', `Black officers`] +
            assignments.overview[Behavior == 'Officers', `Hispanic officers`] +
            assignments.overview[Behavior == 'Officers', `White officers`]
    )
)
cat('\nproportion of black officers with feasible racial comparison:',
    uniqueN(assignments[officer_race == 'officer_black', officer_id]) /
        assignments.overview[Behavior == 'Officers', `Black officers`]
)
cat('\nproportion of hisp officers with feasible racial comparison:',
    uniqueN(assignments[officer_race == 'officer_hisp', officer_id]) /
        assignments.overview[Behavior == 'Officers', `Hispanic officers`]
)
cat('\nproportion of white officers with feasible racial comparison:',
    uniqueN(assignments[officer_race == 'officer_white', officer_id]) /
        assignments.overview[Behavior == 'Officers', `White officers`]
)
cat('\n', rep('-', 80), sep = '')
cat('\ntotal number of black-white officer-shift comparisons:',
    sum(assignments.groups$comparisons.bw)
)
cat('\ntotal number of hisp-white officer-shift comparisons:',
    sum(assignments.groups$comparisons.hw)
)
cat('\n', rep('-', 80), sep = '')
cat('\ntotal number of black-white officer-shift comparisons on same date:',
    sum(assignments.groups$comparisons.bw.samedate)
)
cat('\ntotal number of hisp-white officer-shift comparisons on same date:',
    sum(assignments.groups$comparisons.hw.samedate)
)
sink()

## bootstrap
output.path <- file.path('../results/counts',
                         sprintf('bootstrap_counts_race_%04d_%04d.csv',
                                 boot.start,
                                 boot.end
                         )
)

start.time <- Sys.time()
results.boot <- foreach(
    boot = boot.start:boot.end,
    .combine = 'rbind',
    .packages = c('data.table', 'plyr', 'speedglm')
) %dopar% {
    
    ## report progress
    if (boot %% 10 == 0){
        cat(boot, '\n')
    }
    
    ## bootstrap
    if (boot == 0){  # point estimate
        assignments.boot <- assignments
    } else {         # bootstrap
        set.seed(boot)
        officer_id.boot <- data.table(
            officer_id = sample(unique(assignments$officer_id),
                                replace = TRUE
            )
        )
        assignments.boot <- assignments[officer_id.boot,
                                        on = 'officer_id',
                                        allow.cartesian = TRUE
        ]
    }
    
    ## mean counts of each outcome by unit, month, and officer race
    assignments.summary <- means.by.group(assignments.boot, fe = fe, d = d, ys = ys)
    ## differences in officer-race means by unit-month
    ##   then aggregate those differences with and without weighting
    results.diffmeans <- differences.in.race.means(assignments.summary, fe = fe)
    results.diffmeans <- data.table(results.diffmeans)
    setkey(results.diffmeans, treatment, outcome)
    
    ## demean within fe groups (prepare for fixed-effect regressions);
    ##   do it separately by outcome sets to avoid differential NA dropping
    stops.demean <- demean.by.group(
        assignments.boot,
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
        assignments.boot,
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
        assignments.boot,
        id = 'officer_id',
        fe = fe,
        ds = d,
        ys = c(
            ys[grep('force', ys)],
            'months_from_start',
            'months_from_start_sq'
        )
    )
    
    ## now run fixed-effects regressions for each outcome
    ##   with and without additional controls for officer experience
    ## results.ols <- foreach(y = ys, .combine = 'rbind') %do% {
    results.ols <- ldply(
        ys,
        function(y){
            y.type <- strsplit(y, '_')[[1]][1]  # stops/arrests/force
            data <- get(y.type %.% '.demean')
            
            mod.noexp <- speedlm(  # drop white
                get(y) ~
                    0 +
                    officer_black +
                    officer_hisp,
                data
            )
            mod.exp <- speedlm(
                get(y) ~
                    0 +
                    officer_black +
                    officer_hisp +
                    months_from_start +
                    months_from_start_sq,
                data
            )
            data.frame(treatment = c('officer_black',
                                     'officer_hisp'
            ) %.% '_vs_white',
            outcome = y,
            fe.noexp = coef(mod.noexp)[c('officer_black',
                                         'officer_hisp'
            )
            ],
            fe.exp = coef(mod.exp)[c('officer_black',
                                     'officer_hisp'
            )
            ]
            )
        })
    results.ols <- data.table(results.ols)
    setkey(results.ols, treatment, outcome)
    
    ## merge and return
    results <- results.diffmeans[results.ols,]
    cbind(boot = boot, results)
    
}
end.time <- Sys.time()

fwrite(results.boot,
       output.path
)
