##data input/processing for leadership story
##Rob Arthur 11/26/2018
##libraries
require(lubridate)
require(MASS)
require(lubridate)
require(lme4)
require(ggplot2)
require(tidyverse)
require(readr)
#statistics libraries
require(lme4)
require(mgcv)
#Census libraries
require(tigris)
require(acs)
require(sampSurf)
#set options:
options(tibble.width = Inf, scipen = 99)
##Data input
#Modifying read-in code from Roman Rivera's II tutorial:
#https://github.com/invinst/chicago-police-data/blob/master/data/complaints_general-summary.html
oc <- read_csv('../Data/unified_data/fully-unified-data/complaints/officer-filed-complaints__2017-09.csv.gz')
cmpl <- read_csv('../Data/unified_data/fully-unified-data/complaints/complaints-complaints_2000-2016_2016-11.csv.gz') %>%
    mutate(complaint_class = if_else(cr_id %in% oc$cr_id, 'Officer', 'Civilian'),
         beat = as.numeric(beat)) %>%
         unique()
rm(oc)

acc <- read_csv('../Data/unified_data/fully-unified-data/complaints/complaints-accused_2000-2016_2016-11.csv.gz') %>%
        select(cr_id, UID, recommended_finding, final_finding, complaint_category)
vict <- read_csv('../Data/unified_data/fully-unified-data/complaints/complaints-victims_2000-2016_2016-11.csv.gz')
cmplnt <- read_csv('../Data/unified_data/fully-unified-data/complaints/complaints-complainants_2000-2016_2016-11.csv.gz')
wit <- read_csv('../Data/unified_data/fully-unified-data/complaints/complaints-witnesses_2000-2016_2016-11.csv.gz')

prof <- read_csv('../Data/unified_data/fully-unified-data/profiles/final-profiles.csv.gz') %>%
    mutate(gender = recode(gender, MALE = 'Male', FEMALE = 'Female', .default = 'Unknown'),
         race = recode(race, BLACK = 'Black', HISPANIC = 'Hispanic', WHITE = 'White', .default = 'Other'))

unit_ref <- read_csv('../Data/unified_data/fully-unified-data/data-dictionary/unit_reference.csv')

#read in officer rosters
officers <- read_csv("../Data/unified_data/fully-unified-data/profiles/officer-profiles.csv.gz")

#read in settlements
settlements <- read_csv('../Data/unified_data/fully-unified-data/settlements/settlements_1952-2016_2017-01.csv.gz')
settlements$amt <- as.numeric(as.character(gsub(",", "", gsub("\\$", "", settlements$settlement))))

#read in TRR reports
trr <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-main_2004-2016_2016-09.csv.gz")
trr_charges <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-charges_2004-2016_2016-09.csv.gz")
trr_officers <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-officers_2004-2016_2016-09.csv.gz")
trr_subwep <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-subject-weapons_2004-2016_2016-09.csv.gz")
trr_subjects <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-subjects_2004-2016_2016-09.csv.gz")
trr_actions <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-actions-responses_2004-2016_2016-09.csv.gz")
trr_status <- read_csv("../Data/unified_data/fully-unified-data/TRR/TRR-statuses_2004-2016_2016-09.csv.gz")

#modify actions table
trr_max_force <- trr_actions %>%
        group_by(trr_id) %>%
        summarise(max_force = max(action_sub_category, na.rm = T))

trr_actions <- trr_actions %>%
                    mutate(ranked_resist = recode(trr_actions$resistance_type,
                         `Passive Resister` = 1,
                         `Active Resister` = 2,
                         `Assailant Assault` = 3,
                         `Assailant Battery` = 3,
                         `Assailant Assault/Battery` = 3,
                         `Assailant Deadly Force` = 4))
trr_max_resist <- trr_actions %>%
        group_by(trr_id) %>%
        summarise(max_resist = max(ranked_resist, na.rm = T))
trr_max_resist <- filter(trr_max_resist, !is.infinite(max_resist))
#modify weapons
trr_subwep <-   trr_subwep %>%
                    mutate(ranked_weapon = recode(trr_subwep$weapon_type,
                         `FEET` = 1, `MOUTH (SPIT,BITE,ETC)` = 1, `HANDS/FISTS` = 1, `VERBAL THREAT (ASSAULT)` = 1,
                         `BLUNT INSTRUMENT` = 2, `OTHER (SPECIFY)` = 2,
                         `KNIFE/OTHER CUTTING INSTRUMENT` = 3,
                         `FIREARM - REVOLVER` = 4, `FIREARM - RIFLE` = 4, `FIREARM - SEMI-AUTOMATIC` = 4,
                         `FIREARM - SHOTGUN` = 4, `VEHICLE - ATTEMPTED TO STRIKE OFFICER WITH VEHICLE` = 4,
                         `VEHICLE - OFFICER STRUCK WITH VEHICLE` = 4))
trr_max_weapon <- trr_subwep %>%
        group_by(trr_id) %>%
        summarise(max_weapon = max(ranked_weapon, na.rm = T))

#want to roll this all up into one dataset:
trr_tot <- trr %>%
    left_join(select(trr_charges, - sr_no), by = "trr_id") %>%
    full_join(select(trr_officers, c(3:11)), by = "trr_id") %>%
    full_join(trr_subwep, by = "trr_id") %>%
    left_join(select(trr_subjects, c(1:9)), by = c("trr_id", "subject_no")) %>%
    left_join(trr_max_force, by = "trr_id") %>%
    left_join(trr_max_resist, by = "trr_id")

trr_tot <- trr_tot %>% mutate(weapon_type_mod = replace(weapon_type, which(is.na(weapon_type)), "unarmed"))

#with one row per subject
trr_tot <- select(trr_subjects, everything(), subject_injured = injured) %>%
    full_join(select(trr_officers, c(3:11)), by = "trr_id") %>%
    full_join(trr_max_weapon, by = "trr_id") %>%
    left_join(trr_max_force, by = "trr_id") %>%
    left_join(trr_max_resist, by = "trr_id") %>%
    left_join(trr, by = "trr_id")

trr_tot <- trr_tot %>% mutate(weapon_type_mod = replace(max_weapon, which(is.na(max_weapon)), 1))
##Stats for Appeal/Slate article
#
aggregate(max_force ~ race, data = trr_tot, mean)
aggregate(max_resist ~ race, data = trr_tot, mean)
#
prop.table(table(subset(trr_tot, race == "WHITE")$max_resist == 4))
prop.table(table(subset(trr_tot, race == "BLACK")$max_resist == 4))
#
prop.table(table(subset(trr_tot, race == "WHITE" & max_resist == "4")$max_force < 5))
prop.table(table(subset(trr_tot, race == "BLACK" & max_resist == "4")$max_force < 5))

prop.table(table(subset(trr_tot, race == "WHITE" & max_resist == "3")$max_force))
prop.table(table(subset(trr_tot, race == "BLACK" & max_resist == "3")$max_force))

prop.table(table(subset(trr_tot, race == "WHITE" & max_resist == "4")$max_force > 5))
prop.table(table(subset(trr_tot, race == "BLACK" & max_resist == "4")$max_force > 5))
#data
black_subjects <- subset(trr_tot_nona, race == "BLACK"&max_force>0)
white_subjects <- subset(trr_tot_nona, race == "WHITE"&max_force>0)

black_subjects$max_force[black_subjects$max_force > 2 & black_subjects$max_force < 6] <- 3
black_subjects$max_force[black_subjects$max_force == 6] <- 4

white_subjects$max_force[white_subjects$max_force > 2 & white_subjects$max_force < 6] <- 3
white_subjects$max_force[white_subjects$max_force == 6] <- 4

write.csv(table(round(white_subjects$max_force), white_subjects$max_resist), "white_subjects_force_vs_resist.csv")
write.csv(table(round(black_subjects$max_force), black_subjects$max_resist), "black_subjects_force_vs_resist.csv")

#
prop.table(table(subset(trr_tot, race == "WHITE")$injured))
prop.table(table(subset(trr_tot, race == "BLACK")$injured))
aggregate(injured == "Yes" ~ race, data = subset(trr_tot, max_resist == "4"), mean)
##Now model use of force as a function of resistance, etc.
require(lme4)

#start with injury level
trr_tot$test <- trr_tot$subject_injured == "Yes"
mod <- glmer(test ~ age + gender + race + (1|weapon_type_mod) + duty_status + in_uniform + tenure,
            data = trr_tot, family = binomial(logit), nAGQ = 0)

trr_tot$test <- trr_tot$injured == "Yes"
mod <- glmer(test ~ age + gender + race + (1|weapon_type_mod) + duty_status + in_uniform + tenure,
            data = trr_tot, family = binomial(logit), nAGQ = 0)


##Separately, geocoding TRRs with the publicly available 'crimes' dataset.
crimes <- read_csv("../../../Crime_Data/Chicago_Crimes_-_2001_to_present_2'2017.csv")
crimes <- filter(crimes, !is.na(Latitude))
#Remove leading zeroes on crime block numbers.
crimes <- crimes %>% mutate(newblock = gsub("(?<![0-9])0+", "", crimes$Block, perl = TRUE))
#to match, need a compound field in the TRR files
trr_tot <- trr_tot %>% mutate(newdir = substr(direction, 0, 1))
trr_tot <- trr_tot %>% mutate(address = paste(block, newdir, street, sep = " "))
#97% of all calls' locations match crimes.
prop.table(table(trr_tot$address %in% crimes$newblock))
trr_tot$lat <- crimes[match(trr_tot$address, crimes$newblock),]$Latitude
trr_tot$long <- crimes[match(trr_tot$address, crimes$newblock),]$Longitude
#could try to manually geocode the remaining 7000. But I'll leave them, because for now they don't seem to show a time/place/type of offense pattern.

#Build Census (ACS) dataset with 'acs' package
cook_bg = geo.make(state = "IL", county = "Cook", tract = "*", block.group = "*")
bg_totalpop <- acs.fetch(geography = cook_bg, table.number = "B03002", endyear = 2015)
#bg_totalpop@estimate[,22] <- bg_totalpop@estimate[, 3] / bg_totalpop@estimate[, 1]

cook_bg_map <- block_groups("IL", county = "Cook")

race_df <- data.frame(as.character(paste0(as.character(bg_totalpop@geography$state),
                               as.character(sprintf("%03d", bg_totalpop@geography$county)),
                               as.character(sprintf("%06d", bg_totalpop@geography$tract)),
                               as.character(bg_totalpop@geography$blockgroup))),
                        bg_totalpop@estimate[, 3] / bg_totalpop@estimate[, 1],
                        bg_totalpop@estimate[, 4] / bg_totalpop@estimate[, 1])
colnames(race_df) <- c("GEOID", "nhw", "black")
cook_merged <- geo_join(cook_bg_map, race_df, "GEOID", "GEOID")

trr_tot_nona <- filter(trr_tot, !is.na(lat)&!is.infinite(max_force))
coords <- SpatialPoints(trr_tot_nona[, c("long", "lat")])
proj4string(coords) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
by_bg <- over(coords, cook_bg_map)


bg_analysis <- data.frame(table(by_bg$GEOID))

coords <- SpatialPoints(filter(trr_tot_nona, race=="BLACK")[, c("long", "lat")])
proj4string(coords) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
v <- data.frame(table(over(coords, cook_bg_map)$GEOID))
bg_analysis$black <- v[match(bg_analysis$Var1, v$Var1), 2]
bg_analysis$black_pop <- race_df[match(bg_analysis$Var1, race_df$GEOID),]$black
bg_analysis$prop <- bg_analysis$black / bg_analysis$Freq


plot(bg_analysis$black_pop, bg_analysis$prop, xlab = "Black Population in Block Group", ylab = "Use of Force in Block Group")
##eliminate low incident places
v <- subset(bg_analysis, Freq>50)
scatter.smooth(v$black_pop, v$prop, xlab = "Black Population in Block Group", ylab = "Use of Force Against Black Subjects in Block Group",
               lpars = list(col = "red", lwd = 4), pch = 16, col = rgb(0, 0, 0, .4))
abline(a = 0, b = 1, lty = 3, col = "grey", lwd = 4)
abline(h = 0)
abline(v = 0)

##let's examine blocks with low black population but large numbers of incidents
#adding block group back to original file
trr_tot_nona$bg <- by_bg$GEOID
trr_tot_nona$black <- race_df[match(trr_tot_nona$bg, race_df$GEOID),]$black
trr_tot_nona$white <- race_df[match(trr_tot_nona$bg, race_df$GEOID),]$nhw

table(subset(trr_charges, trr_id%in%subset(trr_tot_nona, race=="BLACK"&black<.1)$trr_id)$description)
subset(trr_tot_nona, trr_id%in%subset(trr_charges, description=="RIDING BICYCLE ON SIDEWALK")$trr_id)$race

##attach NHW to trr_tot_nona
trr_tot_nona$bg_nhw <- race_df[match(by_bg$GEOID, race_df$GEOID),2]
#mean imputation:
trr_tot_nona <- trr_tot_nona %>% mutate(bg_nhw = replace_na(bg_nhw, mean(bg_nhw, na.rm=T)))
#many models...
mod <- gam(max_force ~ weapon_type_mod + s(bg_nhw), data = trr_tot_nona)
mod <- lmer(max_force ~ weapon_type_mod + age + bg_nhw + (1|race), data = trr_tot_nona)

trr_tot_nona$test <- trr_tot_nona$subject_injured=="Yes"
mod <- gam(test ~ s(age) + gender + s(max_force) + max_resist + in_uniform +
                 duty_status + weapon_type_mod + race + bg_nhw * race, data = trr_tot_nona, family = binomial(logit))

mod_black <- gam(max_force ~ s(age) + gender + max_resist + in_uniform + duty_status + weapon_type_mod + bg_nhw, data = filter(trr_tot_nona, race == "BLACK"))
mod_white <- gam(max_force ~ s(age) + gender + max_resist + in_uniform + duty_status + weapon_type_mod + bg_nhw, data = filter(trr_tot_nona, race == "WHITE"))

mod <- lmer(max_force ~ age + gender + max_resist * race + in_uniform +
                 duty_status + (1|weapon_type_mod) + bg_nhw, data = trr_tot_nona)


trr_tot_nona$test <- trr_tot_nona$subject_injured=="Yes"
mod <- glmer(test ~ weapon_type_mod + age + race + in_uniform + duty_status + ( 1 | beat) + (1 | UID),
             family = binomial(logit), data = trr_tot_nona, nAGQ = 0)
trr_tot_nona$pred <- predict(mod, type = "response")
trr_tot_nona$pred_nouid <- predict(mod, re.form = ~(1 | UID), type = "response", )

#not terribly inspiring here. Probably significant, but not inspiring.
trr_tot_nona$test <- trr_tot_nona$subject_injured == "Yes"
mod <- glmer(test ~ weapon_type_mod + age + race + in_uniform + duty_status + (1 | beat) + (1 | UID),
             family = binomial(logit), data = trr_tot_nona, nAGQ = 0)
v <- filter(trr_tot_nona, !is.na(trr_tot_nona$race) & !is.na(trr_tot_nona$age) &
            !is.na(trr_tot_nona$UID) & !is.na(trr_tot_nona$test))
v$pred <- predict(mod, v, type = "response")
v$pred_nouid <- predict(mod, v, re.form = ~(1 | UID), type = "response")
v$dif <- v$pred - v$pred_nouid
z <- aggregate(dif ~ UID, data = v, mean)

#also blech
v <- filter(trr_tot_nona, !is.na(trr_tot_nona$race) & !is.na(trr_tot_nona$age) &
            !is.na(trr_tot_nona$UID) & !is.na(trr_tot_nona$test))
trr_tot_nona$test <- trr_tot_nona$race == "BLACK"
mod <- glmer(test ~ weapon_type_mod + age + in_uniform + duty_status + (1 | beat) + (1 | UID),
             family = binomial(logit), data = trr_tot_nona, nAGQ = 0)
v$pred <- predict(mod, v, type = "response")
v$pred_nouid <- predict(mod, v, re.form = ~(1 | UID), type = "response")
v$dif <- v$pred - v$pred_nouid
x <- aggregate(dif ~ UID, data = v, sum)

##Read in contact cards.
con12 <- read.csv("../../ContactCards/ContactCardData2012 (1).csv")
con13 <- read.csv("../../ContactCards/ContactCardData2013.csv")
con14 <- read.csv("../../ContactCards/ContactCardData2014.csv")

#add UIDs
officers$age <- 2012-officers$birth_year
con12$UID <- officers[match(
    paste(con12$X1st.P.O..FIRST.NAME, con12$X1st.P.O..LAST.NAME, con12$X1st.P.O..AGE, con12$X1ST.P.O..RACE, con12$X1ST.P.O..SEX, sep = "-"),
    paste(officers$first_name, officers$last_name, officers$age, officers$race, substr(officers$gender, 0, 1), sep = "-")),]$UID
con12$UID2 <- officers[match(
    paste(con12$X2nd.P.O.FIRST.NAME, con12$X2nd.P.O..LAST.NAME, con12$X2nd.P.O..AGE, con12$X2ND.P.O..RACE, con12$X2ND.P.O..SEX, sep = "-"),
    paste(officers$first_name, officers$last_name, officers$age, officers$race, substr(officers$gender, 0, 1), sep = "-")),]$UID


table(is.na(match(
    paste(con12$X1st.P.O..FIRST.NAME, con12$X1st.P.O..LAST.NAME, con12$X1st.P.O..AGE, con12$X1ST.P.O..RACE, con12$X1ST.P.O..SEX, sep = "-"),
    paste(officers$first_name, officers$last_name, officers$age, officers$race, substr(officers$gender, 0, 1), sep = "-"))))

#calculate frequencies and compare to UOF
con_table12 <- data.frame(table(con12$UID))
v <- data.frame(table(con12$UID2))
con_table12$sec_off <- v[match(con_table12$Var1, v$Var1), 2]
con_table12$sec_off[is.na(con_table12$sec_off)] <- 0
con_table12$totcon <- con_table12$Freq + con_table12$sec_off

v <- data.frame(table(filter(trr_tot, duty_status == "Yes" & year(ymd(trr_date.x)) == "2012")$UID))
con_table12$uof <- v[match(con_table12$Var1, v$Var1), 2]
con_table12$uof[is.na(con_table12$uof)] <- 0
con_table12$uof_rate <- con_table12$uof / con_table12$totcon
con_table12$race <- officers[match(con_table12$Var1, officers$UID),]$race
con_table12$age <- officers[match(con_table12$Var1, officers$UID),]$age

head(filter(con_table12[order(con_table12$uof_rate, decreasing = T),], totcon > 20))
high_freq <- filter(con_table12, totcon>20)
high_uof <- filter(high_freq, uof_rate > .15)

v <- filter(trr_tot_nona, trr_id%in%filter(trr_officers, UID%in%high_uof$Var1)$trr_id)

plot(trr_tot$long, trr_tot$lat, pch = 16, col = rgb(0, 0, 0, .01), xlim = c(-87.9, -87.55), ylim = c(41.65, 42))
points(v$long, v$lat, pch = 16, col = rgb(1, 0, 0, .2))

#more likely to result in subject injury
prop.table(table(v$subject_injured))
prop.table(table(trr_tot$subject_injured))

#more likely to result in subject injury
prop.table(table(v$injured))
prop.table(table(trr_tot$injured))

#subjects more likely to be black
prop.table(table(v$race))
prop.table(table(trr_tot$race))

prop.table(table(filter(con12, UID %in% high_uof$Var1)$SUBJECT.RACE))
prop.table(table(filter(con12, UID %in% high_freq$Var1)$SUBJECT.RACE))

#more likely to be young
mean(filter(con12, UID %in% high_uof$Var1)$SUBJECT.AGE, na.rm = T)
mean(filter(con12, UID %in% high_freq$Var1)$SUBJECT.AGE, na.rm = T)

mean(filter(con12, UID %in% high_uof$Var1)$SUBJECT.AGE<18, na.rm = T)
mean(filter(con12, UID %in% high_freq$Var1)$SUBJECT.AGE < 18, na.rm = T)

#less likely to be armed
prop.table(table(v$armed))
prop.table(table(trr_tot$armed))

#These are not high risk encounters. Charges resulting from these 
#encounters were more likely to be for minor offenses like resisting arrest
#and possession.
tail(sort(prop.table(table(filter(trr_charges, trr_id%in%filter(trr_tot, UID%in%high_uof$Var1)$trr_id)$description))), 10)
tail(sort(prop.table(table(filter(trr_charges, trr_id %in% filter(trr_tot, UID %in% high_freq$Var1)$trr_id)$description))), 10)

#these officers were *astoundingly* more likely to result in settlements...
sum(filter(settlements, UID %in% high_uof$Var1)$amt)
sum(filter(settlements, UID %in% high_freq$Var1)$amt)

sum(filter(settlements, UID %in% high_uof$Var1)$amt) / nrow(high_uof)
sum(filter(settlements, UID %in% high_uof$Var1)$amt) / nrow(high_freq)

#now 2013
#add UIDs
officers$age <- 2013 - officers$birth_year
con13$UID <- officers[match(
    paste(con13$X1st.P.O..FIRST.NAME, con13$X1st.P.O..LAST.NAME, con13$X1st.P.O..AGE, con13$X1ST.P.O..RACE, con13$X1ST.P.O..SEX, sep = "-"),
    paste(officers$first_name, officers$last_name, officers$age, officers$race, substr(officers$gender, 0, 1), sep = "-")),]$UID
con13$UID2 <- officers[match(
    paste(con13$X2nd.P.O.FIRST.NAME, con13$X2nd.P.O..LAST.NAME, con13$X2nd.P.O..AGE, con13$X2ND.P.O..RACE, con13$X2ND.P.O..SEX, sep = "-"),
    paste(officers$first_name, officers$last_name, officers$age, officers$race, substr(officers$gender, 0, 1), sep = "-")),]$UID

con_table13 <- data.frame(table(con13$UID))
v <- data.frame(table(con13$UID2))
con_table13$sec_off <- v[match(con_table13$Var1, v$Var1), 2]
con_table13$sec_off[is.na(con_table13$sec_off)] <- 0
con_table13$totcon <- con_table13$Freq + con_table13$sec_off

v <- data.frame(table(filter(trr_tot, duty_status == "Yes" & year(ymd(trr_date.x)) == "2013")$UID))
con_table13$uof <- v[match(con_table13$Var1, v$Var1), 2]
con_table13$uof[is.na(con_table13$uof)] <- 0
con_table13$uof_rate <- con_table13$uof / con_table13$totcon
con_table13$race <- officers[match(con_table13$Var1, officers$UID),]$race
con_table13$age <- officers[match(con_table13$Var1, officers$UID),]$age

head(filter(con_table13[order(con_table13$uof_rate, decreasing = T),], totcon > 20))
high_freq <- filter(con_table13, totcon > 20)
high_uof <- filter(high_freq, uof_rate > .15)

v <- filter(trr_tot_nona, trr_id %in% filter(trr_officers, UID %in% high_uof$Var1)$trr_id)

plot(trr_tot$long, trr_tot$lat, pch = 16, col = rgb(0, 0, 0, .01), xlim = c(-87.9, -87.55), ylim = c(41.65, 42))
points(v$long, v$lat, pch = 16, col = rgb(1, 0, 0, .2))

#more likely to result in subject injury
prop.table(table(v$subject_injured))
prop.table(table(trr_tot$subject_injured))

#less like for cop to be injured
prop.table(table(v$injured))
prop.table(table(trr_tot$injured))

#subjects more likely to be black
prop.table(table(v$race))
prop.table(table(trr_tot$race))

prop.table(table(filter(con13, UID %in% high_uof$Var1)$SUBJECT.RACE))
prop.table(table(filter(con13, UID %in% high_freq$Var1)$SUBJECT.RACE))

#more likely to be young
mean(filter(con13, UID %in% high_uof$Var1)$SUBJECT.AGE, na.rm = T)
mean(filter(con13, UID %in% high_freq$Var1)$SUBJECT.AGE, na.rm = T)

mean(filter(con13, UID %in% high_uof$Var1)$SUBJECT.AGE < 18, na.rm = T)
mean(filter(con13, UID %in% high_freq$Var1)$SUBJECT.AGE < 18, na.rm = T)


########now BBQ Becky research
#read in susper calls:
require(sampSurf)
susper <- read_csv("../Data/SuspiciousPersonsCalls.csv")
susper <- susper %>% filter(!is.na(lat)) %>% filter(year(mdy_hms(RecdDate))<2017)
uofcoords <- SpatialPoints(trr_tot_nona[, c("lat", "long")])
proj4string(uofcoords) <- CRS("+proj=cea +datum=WGS84")
trr_tot_nona$datetime <- ymd_hms(paste(trr_tot_nona$trr_date.x, trr_tot_nona$trr_time))
v <- vector()
y <- vector()
z <- vector()
post_susper_uof <- trr_tot_nona[1,]
for (i in 1:nrow(susper)) {
    g <- spCircle(radius = 0.014, centerPoint = c(x = as.numeric(unlist(susper[i, c("lat")])),
        y = as.numeric(unlist(susper[i, c("long")]))))
    proj4string(g$spCircle) <- CRS("+proj=cea +datum=WGS84")
    print(i)
    a <- uofcoords[which((trr_tot_nona$datetime > (mdy_hms(susper$RecdDate[i]) - minutes(1))) &
                         (trr_tot_nona$datetime < (mdy_hms(susper$RecdDate[i]) + hours(1)))),]
    post_susper_uof <- rbind(post_susper_uof, subset(trr_tot_nona, (trr_tot_nona$datetime > (mdy_hms(susper$RecdDate[i]) + minutes(1))) &
                         (trr_tot_nona$datetime < (mdy_hms(susper$RecdDate[i]) + hours(2)))))
    if (nrow(a@coords) == 0) { v[i] <- 0 }
    if (nrow(a@coords) > 0) { v[i] <- sum(over(a, g$spCircle), na.rm = T) }
    a <- uofcoords[which((trr_tot_nona$race=="WHITE") &
                         (trr_tot_nona$datetime > (mdy_hms(susper$RecdDate[i]) - minutes(1))) &
                         (trr_tot_nona$datetime < (mdy_hms(susper$RecdDate[i]) + hours(1)))),]
    if (nrow(a@coords) == 0) { z[i] <- 0}
    else { z[i] <- sum(over(a, g$spCircle), na.rm = T) }
#    b <- sample(1:nrow(susper), 1)
#    g <- spCircle(radius = 0.014, centerPoint = c(x = as.numeric(unlist(susper[b, c("lat")])),
#        y = as.numeric(unlist(susper[b, c("long")]))))
    proj4string(g$spCircle) <- CRS("+proj=cea +datum=WGS84")
    a <- uofcoords[which((trr_tot_nona$datetime > (mdy_hms(susper$RecdDate[i]) - hours(1))) &
                         (trr_tot_nona$datetime < (mdy_hms(susper$RecdDate[i]) - minutes(1)))),]
    if (nrow(a@coords) == 0) { y[i] <- 0 }
    else { y[i] <- sum(over(a, g$spCircle), na.rm = T) }
    }
susper_uof <- data.frame(within = v, white = z, outside = y)
colMeans(susper_uof)
##same idea, but with contact cards
#try to geocode some of these contacts...
con12$blockadd <- paste(con12$ST.NUM, con12$DIR, con12$STREET.NAME, sep = " ")
#lot of 'em in there.
con12$lat <- trr_tot_nona[match(con12$blockadd, trr_tot_nona$address),]$lat
con12$long <- trr_tot_nona[match(con12$blockadd, trr_tot_nona$address),]$long
#
con14$blockadd <- paste(con14$Street.Number, con14$Street.Direction, con14$Street.Name, sep = " ")
#lot of 'em in there.
con14$lat <- trr_tot_nona[match(con14$blockadd, trr_tot_nona$address),]$lat
con14$long <- trr_tot_nona[match(con14$blockadd, trr_tot_nona$address),]$long

require(sampSurf)
susper <- filter(susper, year(mdy_hms(RecdDate))==2014)
concoords <- SpatialPoints(na.omit(con14[, c("lat", "long")]))
proj4string(concoords) <- CRS("+proj=cea +datum=WGS84")
con14$datetime <- dmy_hm(paste(con14[, 1], con14$Time.of.Stop))
con14_mod <- subset(con14, !is.na(lat))
v <- vector()
y <- vector()
z <- vector()
post_susper_con <- con14[1,]
for (i in 1:nrow(susper)) {
    g <- spCircle(radius = 0.014, centerPoint = c(x = as.numeric(unlist(susper[i, c("lat")])),
        y = as.numeric(unlist(susper[i, c("long")]))))
    proj4string(g$spCircle) <- CRS("+proj=cea +datum=WGS84")
    print(i)
    a <- concoords[which((con14_mod$datetime > (mdy_hms(susper$RecdDate[i]) - minutes(1))) &
                         (con14_mod$datetime < (mdy_hms(susper$RecdDate[i]) + hours(1)))),]
    post_susper_con <- rbind(post_susper_con, subset(con14_mod, (con14_mod$datetime > (mdy_hms(susper$RecdDate[i]) + minutes(1))) &
                         (con14_mod$datetime < (mdy_hms(susper$RecdDate[i]) + hours(2)))))
    if (nrow(a@coords) == 0) { v[i] <- 0 }
    else { v[i] <- sum(over(a, g$spCircle), na.rm = T) }
    a <- concoords[which((con14_mod$SubRace == "WHITE") &
                         (con14_mod$datetime > (mdy_hms(susper$RecdDate[i]) - minutes(1))) &
                         (con14_mod$datetime < (mdy_hms(susper$RecdDate[i]) + hours(1)))),]
    if (nrow(a@coords) == 0) { z[i] <- 0 }
    else { z[i] <- sum(over(a, g$spCircle), na.rm = T) }
    b <- sample(1:nrow(susper), 1)
    g <- spCircle(radius = 0.014, centerPoint = c(x = as.numeric(unlist(susper[b, c("lat")])),
        y = as.numeric(unlist(susper[b, c("long")]))))
    proj4string(g$spCircle) <- CRS("+proj=cea +datum=WGS84")
    a <- concoords[which((con14_mod$datetime > (mdy_hms(susper$RecdDate[i]) - minutes(1))) &
                         (con14_mod$datetime < (mdy_hms(susper$RecdDate[i]) + hours(1)))),]
    if (nrow(a@coords) == 0) { y[i] <- 0 }
    else { y[i] <- sum(over(a, g$spCircle), na.rm = T) }
    }
susper_con <- data.frame(within = v, white = z, outside = y)
post_susper_con <- post_susper_con[2:nrow(post_susper_con),]
mean(susper_con$white) /
    mean(susper_con$within)

mean(susper_con$within) /
mean(susper_con$outside)


##Who gets stopped?
prop.table(table(grepl("HOODIE", subset(con14, SubRace == "BLACK")$SubClothing.Description)))
prop.table(table(grepl("HOODIE", subset(post_susper_con, SubRace == "BLACK")$SubClothing.Description)))

prop.table(table(grepl("WHITE T", subset(post_susper_con, SubRace == "BLACK")$SubClothing.Description)))
prop.table(table(grepl("WHITE T", subset(con14, SubRace == "BLACK")$SubClothing.Description)))

mean(subset(post_susper_con, SubRace == "BLACK")$SubAge)
mean(subset(con14, SubRace == "BLACK")$SubAge)





