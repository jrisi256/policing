library(plyr)
library(lubridate)
library(chron)
library(data.table)

library(Matrix)
library(areal)

library(ggplot2)
library(gridExtra)

library(ggmap)
library(cowplot)
library(RColorBrewer)
library(sf)
library(xtable)

options(stringsAsFactors = FALSE)

source('functions.R')



###############
## constants ##
###############

red <- '#A31F34'
orange <- '#CB963F'
blue <- '#4F6F9C'
green <- '#165200'

dir.create('../results/map')
data.dir <- '../data'

chicago.coord <- c(lon = -87.6298, lat = 41.8781)

latdiff.to.km <- function(latdiff){
  110.574 * latdiff
}
londiff.to.km <- function(londiff, lat){
  111.320 * londiff * cos(unname(lat) / 180 * pi)
}

## all of district 2
bbox <- c(left = -87.63141,
          bottom = 41.78406,
          right = -87.57516,
          top = 41.83858
)



#############################################
## load officer background and assignments ##
#############################################

dir.create('../results/checks')
dir.create('../results/summary')
dir.create('../results/districts')
dir.create('../results/assignments')
dir.create('../results/counts')

## officer background information (only use race, gender, appointed date)
officers <- fread('gzip -dc ../data/officers.csv.gz')

## beat assignments
assignments <- fread('gzip -dc ../data/assignments.csv.gz')
assignments[, date := ymd(date)]
assignments[, month := ymd(month)]



############
## checks ##
############

## subset to most common rank (need shared rank for common pool assumption)
##   POLICE OFFICER is 88% of beat assignments
##   SERGEANT is next (8% of beat assignments but very few stops/arrests)
sink('../results/checks/prop_by_officer_rank.log')
round(
  sort(
    prop.table(
      table(assignments$rank)
    )
  ),
  4
)
sink()

## proportion of officers in patrol assignment data, by racial group
sink('../results/checks/prop_by_officer_race.log')
as.matrix(
  cumsum(
    sort(
      prop.table(table(
        officers[officer_id %in% assignments$officer_id, officer_race]
      )),
      decreasing = TRUE
    )
  )
)
sink()



######################
## subset and merge ##
######################

## drop all but POLICE OFFICER
assignments <- assignments[rank == 'POLICE OFFICER',]

## merge officer characteristics
assignments <- assignments[
  officers[,.(officer_id,
              birth_year,
              officer_race,
              officer_gender,
              spanish
              )
           ],
  on = 'officer_id'
]
## drop officers for whom no assignments were found
assignments <- assignments[!is.na(date),]
## drop all officers except black, hispanic, and white
assignments <- assignments[
  officer_race %chin% ('officer_' %.% c('black', 'white', 'hisp')),
  ]



################################################
## assignments summary stats (verifying S1.6) ##
################################################

## load known geographic beats (geobeats)
beats <- st_read(
  dsn = file.path("../data/",
                  'beat_shape'
  ),
  layer = 'geo_export_d5819960-7491-4022-9aaf-63f6a431f4f9'
)$beat_num
beats <- as.numeric(as.character(beats))

# unique patrol tasks (~6k)
length(unique(assignments$beat_assigned))
# map patrol tasks to geobeats (51% are geobeats)
num_beats <- as.numeric(gsub("[^0-9]", "", assignments$beat_assigned))
mean(num_beats %in% beats, na.rm=T)
# beat 1431 (~6k shift slots)
nrow(assignments[grepl("1431", beat_assigned)])
# beat 1431 regular vs relief shift (~4k reg, ~2k relief)
table(assignments[grepl("1431", beat_assigned)]$beat_assigned)
# desk duty, about 8.6%
ba <- gsub("[^0-9]","", assignments$beat_assigned)
mean(ifelse(nchar(ba)==4, substr(ba, 3,4)=="02", substr(ba,2,3)=="02"))

## shift length
assignments[, shiftlength := end_time - start_time]
shiftlength.tab <- table(round(assignments[, shiftlength], 1))

##
plot(
  density(
    assignments[beat_assigned == '1431' & shift == 2, start_time],
    na.rm = TRUE
  ),
  col = 'green',
  xlim = c(0, 36),
  main = 'Shift overlap in 1431 and 1431R'
)
lines(
  density(
    assignments[beat_assigned == '1431' & shift == 2, end_time],
    na.rm = TRUE
  ),
  col = 'green',
  lty = 2
)
lines(
  density(
    assignments[beat_assigned == '1431' & shift == 3, start_time],
    na.rm = TRUE
  ),
  col = 'red'
)
lines(
  density(
    assignments[beat_assigned == '1431' & shift == 3, end_time],
    na.rm = TRUE
  ),
  col = 'red',
  lty = 2
)
lines(
  density(
    assignments[beat_assigned == '1431R', start_time],
    na.rm = TRUE
  ),
  col = 'blue'
)
lines(
  density(
    assignments[beat_assigned == '1431R', end_time],
    na.rm = TRUE
  ),
  col = 'blue',
  lty = 2
)
abline(v = c(6, 12, 18, 24, 30))

assignments[beat_assigned == '1431', mean(shiftlength, na.rm = TRUE)]
assignments[beat_assigned == '1431R', mean(shiftlength, na.rm = TRUE)]
assignments[beat_assigned == '1431', table(shift)]
assignments[beat_assigned == '1431R', table(shift)]


## Of the officer-shifts analyzed, 86% are 9 hours in duration, with 8.5- and
## 8-hour shifts making up an additional 8% and 5%, respectively.
shiftlength.tab['9'] / sum(shiftlength.tab)
shiftlength.tab['8.5'] / sum(shiftlength.tab)
shiftlength.tab['8'] / sum(shiftlength.tab)

assignments.shiftlength <- demean.by.group(
  assignments,
  id = 'officer_id',
  fe = c('beat_assigned', 'weekday', 'shift', 'month'),
  ds = 'officer_race',
  ys = 'shiftlength'
)
mod.shiftlength <- lm(shiftlength ~
                        officer_black +
                        officer_hisp,
                      assignments.shiftlength
                      )
## shifts of Black officers are 0.007 hours shorter (roughly 0.1\% shorter)
coef(mod.shiftlength)
coef(mod.shiftlength)['officer_black']
coef(mod.shiftlength)['officer_black'] /
  mean(assignments$shiftlength, na.rm = TRUE)



###########################
## load officer behavior ##
###########################

## stops
stops <- fread(file.path('../data', 'stops.csv.gz'))
stops[, date := ymd(date)]

## arrests
arrests <- fread(file.path('../data', 'arrests.csv.gz'))
arrests[, date := ymd(date)]

## use of force
force <- fread(file.path('../data', 'force.csv.gz'))
force[, date := ymd(date)]

## stops, first police officer only
stops.1 <- fread(file.path('../data', 'stops.csv.gz'))
stops.1 <- stops.1[po_first == 1]
stops.1[, date := ymd(date)]



#########################################################################
## tabulate stops/arrests/force/complaints by group (officer x period) ##
#########################################################################

## sort for merging
setkey(assignments, officer_id, date)
setkey(stops, officer_id, date)
setkey(stops.1, officer_id, date)
setkey(arrests, officer_id, date)
setkey(force, officer_id, date)

## subset to stops occurring during recorded shift
## step 1: match officer_id, match stop date to date of shift start
stops.merged <- stops[assignments]
stops.merged <- stops.merged[  # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
## step 2: match officer_id, deal with stops past midnight on overnight shifts
assignments.nextday <- assignments[end_time > 24]  # find overnight shifts
assignments.nextday[, start_time := 0]
assignments.nextday[, end_time := end_time - 24]
assignments.nextday[, date_nextday := date]        # copy date...
day(assignments.nextday$date_nextday) <-
  day(assignments.nextday$date_nextday) + 1        # ... and increment forward
setkey(assignments.nextday, officer_id, date_nextday)
stops.merged.nextday <- stops[assignments.nextday,
                             ,
                              on = c(officer_id = 'officer_id', date = 'date_nextday')
                              ]
stops.merged.nextday <- stops.merged.nextday[ # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
stops.merged.nextday[, date := i.date]
stops.merged.nextday[, i.date := NULL]
stops.merged.nextday[, hour := hour + 24]
## merge same-day stops with next-day stops on overnight shifts
stops.merged <- rbind(stops.merged, stops.merged.nextday)
stops.merged <- unique(stops.merged)
## tabulate stops by group (officer x period)
##   including broken down by various stop types (reason, suspect race)
stops.by.group <- stops.merged[
 ,
   .(stops_n_total = sum(!is.na(stop_id))),
   by = c('officer_id', 'beat_assigned', 'shift', 'date')
]
## counts by contact type
stops.by.group.and.type <- dcast(
  stops.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'contact_type')
               ],
  officer_id + beat_assigned + shift + date ~ contact_type,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.by.group.and.type <- stops.by.group.and.type[, -'NA']
## rename for merge
colnames(stops.by.group.and.type)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(stops.by.group.and.type)
         )
] <- 'stops_n_' %.%
  colnames(stops.by.group.and.type)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(stops.by.group.and.type)
           )
  ]
## counts by civilian race
stops.by.group.and.race <- dcast(
  stops.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'civilian_race_short')
               ],
  officer_id + beat_assigned + shift + date ~ civilian_race_short,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.by.group.and.race <- stops.by.group.and.race[, -'NA']
## rename for merge
colnames(stops.by.group.and.race)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(stops.by.group.and.race)
         )
] <- 'stops_n_' %.%
  colnames(stops.by.group.and.race)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(stops.by.group.and.race)
           )
  ]

## subset to first po stops occurring during recorded shift
## step 1: match officer_id, match stop date to date of shift start
stops.1.merged <- stops.1[assignments,]
stops.1.merged <- stops.1.merged[  # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
## step 2: match officer_id, deal with stops.1 past midnight on overnight shifts
assignments.nextday <- assignments[end_time > 24]  # find overnight shifts
assignments.nextday[, start_time := 0]
assignments.nextday[, end_time := end_time - 24]
assignments.nextday[, date_nextday := date]        # copy date...
day(assignments.nextday$date_nextday) <-
  day(assignments.nextday$date_nextday) + 1        # ... and increment forward
setkey(assignments.nextday, officer_id, date_nextday)
stops.1.merged.nextday <- stops.1[assignments.nextday,
                              ,
                              on = c(officer_id = 'officer_id', date = 'date_nextday')
                              ]
stops.1.merged.nextday <- stops.1.merged.nextday[ # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
stops.1.merged.nextday[, date := i.date]
stops.1.merged.nextday[, i.date := NULL]
stops.1.merged.nextday[, hour := hour + 24]
## merge same-day stops.1 with next-day stops.1 on overnight shifts
stops.1.merged <- rbind(stops.1.merged, stops.1.merged.nextday)
stops.1.merged <- unique(stops.1.merged)
## tabulate stops.1 by officer in each period
##   including broken down by various stop types (reason, suspect race)
stops.1.by.group <- stops.1.merged[
  ,
  .(pofirst_stops_n_total = sum(!is.na(stop_id))),
  by = c('officer_id', 'beat_assigned', 'shift', 'date')
  ]
## counts by contact type
stops.1.by.group.and.type <- dcast(
  stops.1.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'contact_type')
               ],
  officer_id + beat_assigned + shift + date ~ contact_type,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.1.by.group.and.type <- stops.1.by.group.and.type[, -'NA']
## rename for merge
colnames(stops.1.by.group.and.type)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(stops.1.by.group.and.type)
  )
  ] <- 'pofirst_stops_n_' %.%
  colnames(stops.1.by.group.and.type)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(stops.1.by.group.and.type)
    )
    ]
## counts by civilian race
stops.1.by.group.and.race <- dcast(
  stops.1.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'civilian_race_short')
               ],
  officer_id + beat_assigned + shift + date ~ civilian_race_short,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
stops.1.by.group.and.race <- stops.1.by.group.and.race[, -'NA']
## rename for merge
colnames(stops.1.by.group.and.race)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(stops.1.by.group.and.race)
  )
  ] <- 'pofirst_stops_n_' %.%
  colnames(stops.1.by.group.and.race)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(stops.1.by.group.and.race)
    )
    ]

## subset to arrests occurring during recorded shift
## step 1: match officer_id, match arrest date to date of shift start
arrests.merged <- arrests[assignments,]
arrests.merged <- arrests.merged[  # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
## step 2: match officer_id, deal with arrests past midnight on overnight shifts
arrests.merged.nextday <- arrests[assignments.nextday,
                             ,
                              on = c(officer_id = 'officer_id', date = 'date_nextday')
                              ]
arrests.merged.nextday <- arrests.merged.nextday[ # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
arrests.merged.nextday[, date := i.date]
arrests.merged.nextday[, i.date := NULL]
arrests.merged.nextday[, hour := hour + 24]
## merge same-day arrests with next-day arrests on overnight shifts
arrests.merged <- rbind(arrests.merged, arrests.merged.nextday)
arrests.merged <- unique(arrests.merged)

## tabulate arrests by officer in each period
##   including broken down by various arrest types (reason, suspect race)
arrests.by.group <- arrests.merged[
 ,
   .(arrests_n_total = sum(!is.na(arrest_id))),
   by = c('officer_id', 'beat_assigned', 'shift', 'date')
]
## counts by contact type
arrests.by.group.and.crimecode <- dcast(
  arrests.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'crime_code')
               ],
  officer_id + beat_assigned + shift + date ~ crime_code,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
arrests.by.group.and.crimecode <- arrests.by.group.and.crimecode[, -'NA']
## rename for merge
colnames(arrests.by.group.and.crimecode)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(arrests.by.group.and.crimecode)
         )
] <- 'arrests_n_' %.%
  colnames(arrests.by.group.and.crimecode)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(arrests.by.group.and.crimecode)
           )
  ]
## counts by civilian race
arrests.by.group.and.race <- dcast(
  arrests.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'civilian_race_short')
               ],
  officer_id + beat_assigned + shift + date ~ civilian_race_short,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
arrests.by.group.and.race <- arrests.by.group.and.race[, -'NA']
## rename for merge
colnames(arrests.by.group.and.race)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(arrests.by.group.and.race)
         )
] <- 'arrests_n_' %.%
  colnames(arrests.by.group.and.race)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(arrests.by.group.and.race)
           )
  ]



## subset to uses of force occurring during recorded shift
## step 1: match officer_id, match force date to date of shift start
force.merged <- force[assignments,]
force.merged <- force.merged[  # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
## step 2: match officer_id, deal with force past midnight on overnight shifts
force.merged.nextday <- force[assignments.nextday,
                             ,
                              on = c(officer_id = 'officer_id', date = 'date_nextday')
                              ]
force.merged.nextday <- force.merged.nextday[ # drop if not during shift
  is.na(hour) | between(hour, floor(start_time), ceiling(end_time)),
  ]
force.merged.nextday[, date := i.date]
force.merged.nextday[, i.date := NULL]
force.merged.nextday[, hour := hour + 24]
## merge same-day force with next-day force on overnight shifts
force.merged <- rbind(force.merged, force.merged.nextday)
force.merged <- unique(force.merged)
## tabulate force by officer in each period
force.by.group <- force.merged[
 ,
   .(force_n_total = sum(!is.na(force_id)),
     force_n_injury = sum(!is.na(force_id) & civ.injured == 1)
     ),
   by = c('officer_id', 'beat_assigned', 'shift', 'date')
]
## counts by civilian race
force.by.group.and.race <- dcast(
  force.merged[,
               .N,
               by = c('officer_id', 'beat_assigned', 'shift', 'date', 'civilian_race_short')
               ],
  officer_id + beat_assigned + shift + date ~ civilian_race_short,
  value.var = 'N',
  fill = 0
)
## drop NA counts (used to ensure no-event shifts are recorded as zeroes)
force.by.group.and.race <- force.by.group.and.race[, -'NA']
## rename for merge
colnames(force.by.group.and.race)[
  -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
         colnames(force.by.group.and.race)
         )
] <- 'force_n_' %.%
  colnames(force.by.group.and.race)[
    -match(c('officer_id', 'beat_assigned', 'shift', 'date'),
           colnames(force.by.group.and.race)
           )
  ]


## merge stop counts
assignments <- stops.by.group[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
assignments <- stops.by.group.and.type[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
assignments <- stops.by.group.and.race[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]

## merge first po stop counts
assignments <- stops.1.by.group[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
  ]
assignments <- stops.1.by.group.and.type[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
  ]
assignments <- stops.1.by.group.and.race[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
  ]

## merge arrest counts
assignments <- arrests.by.group[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
assignments <- arrests.by.group.and.crimecode[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
assignments <- arrests.by.group.and.race[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
## merge force counts
assignments <- force.by.group[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]
assignments <- force.by.group.and.race[
  assignments,
  on = c('officer_id', 'beat_assigned', 'shift', 'date')
]



## n by officer behavior
sink('../results/summary/dataset_counts.txt')
cat(
  sprintf(
    '%0.2fm stops\n%0.2fm first-po stops\n%0.2fm arrests\n%0.2fm uses of force',
    nrow(stops.merged[!is.na(stop_id),]) / 1e6,
    nrow(stops.1.merged[!is.na(stop_id),]) / 1e6,
    nrow(arrests.merged[!is.na(arrest_id)]) / 1e6,
    nrow(force.merged[!is.na(force_id)]) / 1e6
  )
)
sink()

sum(assignments[, stops_n_total], na.rm = TRUE) +
sum(assignments[, arrests_n_total], na.rm = TRUE) +
sum(assignments[, force_n_total], na.rm = TRUE)

## write out summary stats for period of interest
dataset.overview <-
  data.frame(
    c('Stops', 'Arrests', 'Uses of force', 'Shifts', 'Officers'),
    cbind(
      rbind(table(stops.merged[!is.na(stop_id), officer_race]),
            table(arrests.merged[!is.na(arrest_id), officer_race]),
            table(force.merged[!is.na(force_id), officer_race]),
            table(assignments[, officer_race]),
            table(
              unique(
                assignments[, .(officer_id, officer_race)]
              )$officer_race
            )
            ),
      rbind(table(stops.merged[!is.na(stop_id), officer_gender]),
            table(arrests.merged[!is.na(arrest_id), officer_gender]),
            table(force.merged[!is.na(force_id), officer_gender]),
            table(assignments[, officer_gender]),
            table(
              unique(
                assignments[, .(officer_id, officer_gender)]
              )$officer_gender
            )
            )
    )
  )
colnames(dataset.overview) <- c(
  'Behavior',
  'Black officers',
  'Hispanic officers',
  'White officers',
  'Female officers',
  'Male officers'
  )

fwrite(dataset.overview,
       '../results/summary/dataset_overview.csv'
       )
sink('../results/summary/dataset_overview.tex')
print(
  xtable(
    dataset.overview
  ),
  include.rownames = FALSE
)
sink()

## cleanup
rm(stops, stops.merged.nextday,
   stops.by.group, stops.by.group.and.type, stops.by.group.and.race,
   stops.1, stops.1.merged, stops.1.merged.nextday,
   stops.1.by.group, stops.1.by.group.and.type, stops.1.by.group.and.race,
   arrests, arrests.merged.nextday,
   arrests.by.group, arrests.by.group.and.crimecode, arrests.by.group.and.race,
   force, force.merged.nextday,
   force.by.group, force.by.group.and.race,
   assignments.nextday
   )

fwrite(assignments, '../results/assignments_with_outcomes.csv')



###################
## summary stats ##
###################

nrow(assignments)

sink('../results/summary/shifts_by_group.txt')
cat('\n\nfemale:\n')
prop.table(table(assignments[officer_gender == 'FEMALE', shift]))
cat('\n\nmale:\n')
prop.table(table(assignments[officer_gender == 'MALE', shift]))
cat('\n\nblack:\n')
prop.table(table(assignments[officer_race == 'officer_black', shift]))
cat('\n\nhispanic:\n')
prop.table(table(assignments[officer_race == 'officer_hisp', shift]))
cat('\n\nwhite:\n')
prop.table(table(assignments[officer_race == 'officer_white', shift]))
sink()



###############
## load maps ##
###############

## load map
base.map <- get_stamenmap(
  bbox = bbox,
  zoom = 14,
  maptype = 'toner-lite'
)

## load beat boundaries
beat.shape <- st_read(
  dsn = file.path(data.dir,
                  'beat_shape'
  ),
  layer = 'geo_export_d5819960-7491-4022-9aaf-63f6a431f4f9'
)
beat.crs <- st_crs(beat.shape)
beat.shape <- st_transform(beat.shape, crs = 3435)

## load district boundaries
district.shape <- st_read(
  dsn = file.path(data.dir,
                  'district_shape',
                  'Boundaries - Police Districts (current)'
  ),
  layer = 'geo_export_427fc6e8-19e2-4435-b9e4-0566a20fa39c'
)

## illinois census block group (bg) boundaries
census.shape <- st_read(
  dsn = file.path(data.dir,
                  'census_blockgroup_shape'
  ),
  layer = 'cb_2018_17_bg_500k'
)
census.shape <- st_transform(census.shape, crs = 3435)



######################
## load census data ##
######################

## load chicago census bg racial population
census.race <- fread(
  file.path(data.dir, 'census_blockgroup_data/DEC_10_SF1_P11_with_ann.csv'),
  skip = 2
)
colnames(census.race) <- as.character(fread(
  file.path(data.dir, 'census_blockgroup_data/DEC_10_SF1_P11_with_ann.csv'),
  nrows = 1,
  header = FALSE
))
census.race <- census.race[
  ,
  .(bg = as.character(GEO.id2),
    pop = D001,
    pop_hisp = D002,
    pop_white = D005,
    pop_black = D006
  )
  ]
## drop bgs outside of cook county and merge
census.shape <- census.shape[census.shape$GEOID %chin% census.race$bg,]
census.shape <- merge(census.shape, census.race, by.x = 'GEOID', by.y = 'bg')



##################################
## load officer characteristics ##
##################################

## rname events
stops <- stops.merged
stops$month <- ymd(stops$month)
arrests <- arrests.merged
arrests$month <- ymd(arrests$month)
force <- force.merged
force$month <- ymd(force$month)
rm(stops.merged, arrests.merged, force.merged)



##################################
## subset to period of interest ##
##################################

district.of.interest <- 2

months.of.interest <- ymd('2012-' %.% 10:12 %.% '-01')

assignments.of.interest <- assignments[#dk
  unit == district.of.interest &
    month %in% months.of.interest &
    grepl('^[0-9]', beat_assigned),
  ]

## stops
stops.of.interest <-
  stops[month %in% months.of.interest & district == district.of.interest]
stops.of.interest[, time := ymd_hms(time)]
## for plotting, jitter by 100 meters
set.seed(02139)
stops.of.interest[,
                  lat.jitter := jitter(lat,
                                       amount = 0.0009043717
                                       )
                  ]
stops.of.interest[,
                  lon.jitter := jitter(lon,
                                       amount = 0.001206488
                                       )
                  ]



## arrests in bbox
arrests.of.interest <-
  arrests[month %in% months.of.interest & district == district.of.interest]
arrests.of.interest[, time := ymd_h(paste0(date, '_', hour))]
## for plotting, jitter by 100 meters
set.seed(02139)
arrests.of.interest[,
                    lat.jitter := jitter(lat,
                                         amount = 0.0009043717
                                         )
                    ]
arrests.of.interest[,
                    lon.jitter := jitter(lon,
                                         amount = 0.001206488
                                         )
                    ]



## force in district
force.of.interest <-
  force[month %in% months.of.interest & district == district.of.interest]
force.of.interest[, time := ymd_hms(time)]
## for plotting, jitter by 100 meters
set.seed(02139)
force.of.interest[,
                  lat.jitter := jitter(lat,
                                       amount = 0.0009043717
                                       )
                  ]
force.of.interest[,
                  lon.jitter := jitter(lon,
                                       amount = 0.001206488
                                       )
                  ]



################
## preprocess ##
################

## compute beat racial composition by spatial merge with census
##   while handling bgs falling into multiple beats
##   (assume pop within bg is uniformly distributed)
beat.shape <- aw_interpolate(
  beat.shape,
  tid = beat_num,
  source = census.shape,
  sid = GEOID,
  weight = 'sum',
  output = 'sf',
  extensive = c('pop', 'pop_black', 'pop_hisp', 'pop_white')
)
beat.shape$prop_white <- beat.shape$pop_white / beat.shape$pop
beat.shape$prop_white <- ifelse(is.na(beat.shape$prop_white),
                                0,
                                beat.shape$prop_white
)

## now convert beat map back for plotting
beat.shape <- st_transform(beat.shape, crs = st_crs(beat.crs))



###############
## make maps ##
###############

scale <- 3

stop.factor <- factor('Stop',
                      levels = c('Stop', 'Arrest', 'Force'),
                      ordered = TRUE
)
arrest.factor <- factor('Arrest',
                        levels = c('Stop', 'Arrest', 'Force'),
                        ordered = TRUE
)
force.factor <- factor('Force',
                       levels = c('Stop', 'Arrest', 'Force'),
                       ordered = TRUE
)

beat.df <- lapply(1:nrow(beat.shape),
              function(i){
                out <- as.data.frame(st_geometry(beat.shape)[[i]][[1]])
                out <- rbind(out, out[1,])
                colnames(out) <- c('x', 'y')
                out$prop_white <- beat.shape$prop_white[i]
                out$index <- i
                out
              })
beat.df <- rbindlist(beat.df)
beat.df[, x := pmax(x, bbox['left'])]
beat.df[, x := pmin(x, bbox['right'])]
beat.df[, y := pmax(y, bbox['bottom'])]
beat.df[, y := pmin(y, bbox['top'])]



pdf('../results/map/map.pdf',
    width = scale * londiff.to.km(bbox['right'] - bbox['left'],
                                  lat = chicago.coord['lat']
                                  ),
    height = scale * latdiff.to.km(bbox['top'] - bbox['bottom'])
    )
ggmap(base.map, extent = 'device') +
  geom_rect(xmin = bbox['left'],
            xmax = bbox['right'],
            ymin = bbox['bottom'],
            ymax = bbox['top'],
            color = 'black',
            fill = NA
            ) +
  geom_polygon(aes(x = x,
                   y = y,
                   group = index,
                   alpha = 1 - prop_white
                   ),
               data = beat.df,
               fill = 'black',
               color = 'black'
               ) +
  scale_alpha_continuous(guide = FALSE, range = c(0, .65)) +
  scale_color_manual(name = 'behavior',
                     values = c(Stop = green,
                                Arrest = blue,
                                Force = red
                                )
                     ) +
  scale_fill_manual(name = 'behavior',
                    values = c(Stop = green,
                               Arrest = blue,
                               Force = red
                               )
                    ) +
  scale_shape_manual(name = 'behavior',
                     values = c(Stop = 21,
                                Arrest = 0,
                                Force = 4
                                )
                     ) +
  scale_size_manual(name = 'behavior',
                    values = c(Stop = 4,
                               Arrest = 5,
                               Force = 6
                               )
                    ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 fill = stop.factor,
                 shape = stop.factor,
                 size = stop.factor
                 ),
             data = stops.of.interest[stop_id != 435850,],
             color = 'white',
             stroke = .25
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 shape = arrest.factor,
                 size = arrest.factor
                 ),
             data = arrests.of.interest[arrest_id != 1118284,],
             color = 'gray',
             stroke = 2.5
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 color = arrest.factor,
                 shape = arrest.factor,
                 size = arrest.factor
                 ),
             data = arrests.of.interest[arrest_id != 1118284,],
             stroke = 2
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 shape = force.factor,
                 size = force.factor
                 ),
             color = 'white',
             data = force.of.interest[force_id != 48008,],
             stroke = 4.5
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 color = force.factor,
                 shape = force.factor,
                 size = force.factor
                 ),
             data = force.of.interest[force_id != 48008,],
             stroke = 4
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter
                 ),
             data = stops.of.interest[stop_id == 435850,],
             shape = 0,
             size = 18,
             stroke = 4
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter
                 ),
             data = arrests.of.interest[arrest_id == 1118284 & officer_id == 14681,],
             shape = 0,
             size = 18,
             stroke = 4
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter
                 ),
             data = force.of.interest[force_id == 48008,],
             shape = 0,
             size = 18,
             stroke = 4
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 fill = stop.factor
                 ),
             data = stops.of.interest[stop_id == 435850,],
             shape = 21,
             stroke = .5,
             size = 12,
             color = 'white'
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 fill = arrest.factor
                 ),
             data = arrests.of.interest[arrest_id == 1118284 & officer_id == 14681,],
             shape = 22,
             stroke = .25,
             size = 12,
             color = 'gray'
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 shape = force.factor
                 ),
             data = force.of.interest[force_id == 48008,],
             stroke = 6.5,
             color = 'white',
             size = 8
             ) +
  geom_point(aes(x = lon.jitter,
                 y = lat.jitter,
                 color = force.factor,
                 shape = force.factor
                 ),
             data = force.of.interest[force_id == 48008,],
             stroke = 6,
             size = 8
             ) +
  cowplot::theme_nothing()
dev.off()



officers.to.plot <- c(14681,
                      3771,
                      30911,
                      28969
)
officers.to.labels  <- c('14681' = 'A',
                         '3771' = 'B',
                         '30911' = 'C',
                         '28969' = 'D'
)

beats.to.plot <- sort(unique(
  assignments.of.interest$beat_assigned
))
beats.to.plot <- gsub('^0', '', beats.to.plot)
beats.to.plot <- beats.to.plot[grep('^2', beats.to.plot)]
beats.to.plot <- setdiff(beats.to.plot, '2REV')

assignments.to.plot <- assignments.of.interest[officer_id %in% officers.to.plot,]
assignments.to.plot[, officer.label := officers.to.labels[as.character(officer_id)]]
assignments.to.plot[, beat_assigned := factor(beat_assigned,
                                              levels = rev(beats.to.plot),
                                              labels = 'Beat ' %.% rev(beats.to.plot),
                                              ordered = TRUE
)
]
assignments.to.plot$date <- ymd(assignments.to.plot$date)

dates.to.plot <- sort(unique(assignments[month %in% months.of.interest, date]))
weekends.to.plot <- data.table(
  date = dates.to.plot[is.weekend(dates.to.plot)]
)
weekends.to.plot[, date := ymd(date)]
weekends.to.plot[, datetime.start := ymd_h(paste(date, 0))]
weekends.to.plot[, datetime.end := ymd_h(paste(date, 24))]

behavior.to.plot <- rbind(
  cbind(type = 'Stop', stops.of.interest[officer_id %in% officers.to.plot,]),
  cbind(type = 'Arrest', arrests.of.interest[officer_id %in% officers.to.plot,]),
  cbind(type = 'Force', force.of.interest[officer_id %in% officers.to.plot,]),
  fill = TRUE
)
behavior.to.plot[, officer_gender := tolower(officer_gender)]
behavior.to.plot[, officer_race := gsub('officer_', '', officer_race)]
## generate labels
behavior.to.plot[
  ,
  officer.short := officers.to.labels[as.character(officer_id)]
  ]
behavior.to.plot[
  ,
  officer.label := sprintf('Officer %s:\nb. %s,\n%s,\n%s,\n%s',
                           officers.to.labels[as.character(officer_id)],
                           year(appointed_month),
                           tolower(officer_gender),
                           ifelse(officer_race=='hisp', 'Hispanic', officer_race),
                           ifelse(spanish, 'Spanish', 'no Spanish')
  )
  ]
behavior.to.plot[, officer.label.1line := gsub('\n', ' ', officer.label)]
behavior.to.plot[, officer.label.1line := gsub('b\\.', 'joined CPD', officer.label.1line)]
behavior.to.plot[, type := factor(type,
                                  levels = rev(
                                    c('Stop',
                                      'Arrest',
                                      'Force'
                                    )
                                  ),
                                  ordered = TRUE
)
]


plot.assignments <-
  ggplot(assignments.to.plot) +
  theme_light(base_size = 14) +
  scale_x_date(name = '',
               breaks = ymd(c('2012-10-01',
                              '2012-11-01',
                              '2012-12-01',
                              '2013-01-01'
                              )
                            ),
               labels = c('Oct', 'Nov', 'Dec', 'Jan'),
               minor_breaks = NULL,
               limits = ymd(c('2012-10-01',
                              '2013-01-01'
                              )
                            )
               ) +
  geom_rect(data = weekends.to.plot,
            aes(xmin = date,
                xmax = date + 1
                ),
            ymin = .5,
            ymax = 25.5,
            fill = 'grey92'
            ) +
  ## scale_y_discrete(drop = FALSE) +
  geom_text(aes(x = date,
                y = beat_assigned,
                label = officer.label
                ),
            color = NA
            ) +
  geom_hline(yintercept = seq(-.5, length(beats.to.plot), 1),
             color = 'grey92'
             ) +
  geom_text(aes(x = date,
                y = beat_assigned,
                label = officer.label
                ),
            color = 'grey30'
            ) +
  theme(text = element_text(family = 'Helvetica'),
        axis.text.y = element_text(hjust = 0),
        panel.grid.major = element_blank(),
        axis.title = element_text(color = 'grey30')
        ) +
  ylab('') +
  xlab('')



plot.behavior <-
  ggplot() +
  theme_light(base_size = 14) +
  scale_x_datetime(name = '',
                   breaks = ymd_h(c('2012-10-01 0',
                                    '2012-11-01 0',
                                    '2012-12-01 0',
                                    '2013-01-01 0'
                   )
                   ),
                   labels = c('Oct', 'Nov', 'Dec', 'Jan'),
                   minor_breaks = NULL,
                   limits = ymd_h(c('2012-10-01 0',
                                    '2013-01-01 0'
                   )
                   )
  ) +
  geom_rect(data = weekends.to.plot,
            aes(xmin = datetime.start,
                xmax = datetime.end
            ),
            ymin = .5,
            ymax = 25.5,
            fill = 'grey92'
  ) +
  ## scale_y_discrete(drop = FALSE) +
  geom_point(data = behavior.to.plot,
             aes(x = time,
                 y = type
             ),
             color = NA
  ) +
  geom_hline(yintercept = seq(-.5, 5, 1),
             color = 'grey92'
  ) +
  geom_point(data = behavior.to.plot,
             aes(x = time,
                 y = type,
                 color = type,
                 shape = type
             ),
             size = 2.5,
             stroke = 1
  ) +
  geom_point(aes(x = time,
                 y = type,
                 fill = stop.factor
                 ),
             data = behavior.to.plot[stop_id == 435850,],
             shape = 0,
             stroke = 1 ,
             size = 6
             ) +
  geom_point(aes(x = time,
                 y = type,
                 fill = stop.factor
                 ),
             data = behavior.to.plot[arrest_id == 1118284,],
             shape = 0,
             stroke = 1 ,
             size = 6
             ) +
  geom_point(aes(x = time,
                 y = type,
                 fill = stop.factor
                 ),
             data = behavior.to.plot[force_id == 48008,],
             shape = 0,
             stroke = 1 ,
             size = 6
             ) +
  scale_color_manual(name = 'Behavior',
                     values = c(Stop = green,
                                Arrest = blue,
                                Force = red
                     )
  ) +
  scale_shape_manual(name = 'Behavior',
                     values = c(Stop = 1,
                                Arrest = 0,
                                Force = 4
                     )
  ) +
  theme(text = element_text(family = 'Helvetica'),
        axis.text.y = element_text(hjust = 0),
        strip.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        legend.position = 'none'
  ) +
  ylab('') +
  xlab('') +
  facet_wrap('officer.label.1line', ncol = 1)



plot.legend <-
  ggplot() +
  coord_fixed(xlim = c(-.1, 9.1),
              ylim = c(-.1, 2.1)
              ) +
  theme_void() +
  theme(text = element_text(family = 'Helvetica')) +
  ## stop
  annotate('rect',
           xmin = 0,
           xmax = 3,
           ymin = 0,
           ymax = 2,
           colour = 'gray87',
           fill = 'white'
           ) +
  annotate('point',
           x = 1.5,
           y = 1.75,
           size = 6,
           stroke = 2,
           color = green,
           shape = 1
           ) +
  annotate('text',
           x = .2,
           y = 1.4,
           label = 'Stop by officer B:',
           fontface = 'bold',
           hjust = 0,
           vjust = 1,
           size = 5,
           color = 'grey30'
           ) +
  annotate('text',
           x = 0.2,
           y = 1 ,
           label = 'Time: 2012-11-03, 8:08am\nCivilian race: Black\nReason: Suspicious',
           hjust = 0,
           vjust = 1,
           size = 4,
           color = 'grey30'
           ) +
  ## arrest
  annotate('rect',
           xmin = 3,
           xmax = 6,
           ymin = 0,
           ymax = 2,
           colour = 'gray87',
           fill = 'white'
           ) +
  annotate('point',
           x = 4.5,
           y = 1.75,
           size = 5.5,
           stroke = 2.5,
           color = blue,
           shape = 0
           ) +
  annotate('text',
           x = 3.2,
           y = 1.4,
           label = 'Arrest by officer A:',
           fontface = 'bold',
           hjust = 0,
           vjust = 1,
           size = 5,
           color = 'grey30'
           ) +
  annotate('text',
           x = 3.2,
           y = 1 ,
           label = 'Time: 2012-10-03, 11:00am\nCivilian race: Black\nReason: Drug-related',
           hjust = 0,
           vjust = 1,
           size = 4,
           color = 'grey30'
           ) +
  ## force
  annotate('rect',
           xmin = 6,
           xmax = 9,
           ymin = 0,
           ymax = 2,
           colour = 'gray87',
           fill = 'white'
           ) +
  annotate('point',
           x = 7.5,
           y = 1.75,
           size = 5,
           stroke = 2.5,
           color = red,
           shape = 4
           ) +
  annotate('text',
           x = 6.2,
           y = 1.4,
           label = 'Force by officer D:',
           fontface = 'bold',
           hjust = 0,
           vjust = 1,
           size = 5,
           color = 'grey30'
           ) +
  annotate('text',
           x = 6.2,
           y = 1 ,
           label = 'Time: 2012-10-08, 7:13pm\nCivilian race: Black\nInjury: none',
           hjust = 0,
           vjust = 1,
           size = 4,
           color = 'grey30'
           )
plot.legend


pdf('../results/map/timeline.pdf',
    width = 8.5,
    height = 11
    )
plot_grid(plot.assignments,
          plot.legend,
          plot.behavior,
          ncol = 1,
          align = 'v',
          axis = 'l',
          rel_heights = c(1, .4, 1)
          )
dev.off()
