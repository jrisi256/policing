##data input/processing V2.0
##Rob Arthur 6/11/2018
##libraries
require(lubridate)
require(MASS)
require(lubridate)
require(lme4)
require(ggplot2)
require(sna)
require(igraph)
##reading data in
#all well-behaved csvs
# aw <- read.csv("Data/new_data/awards_1967-2017_2017-08.csv")
# comp_acc <- read.csv("Data/new_data/complaints-accused_2000-2018_2018-03.csv")
# comp_comp <- read.csv("Data/new_data/complaints-complainants_2000-2018_2018-03.csv")
# complaints <- read.csv("Data/new_data/complaints-complaints_2000-2018_2018-03.csv")
# comp_inv <- read.csv("Data/new_data/complaints-investigators_2000-2018_2018-03.csv")
# comp_vic <- read.csv("Data/new_data/complaints-victims_2000-2018_2018-03.csv")
# comp_cpdwit <- read.csv("Data/new_data/complaints-CPD-witnesses_2000-2018_2018-03.csv")
# prof <- read.csv("Data/new_data/final-profiles.csv")
# off_comp <- read.csv("Data/new_data/officer-filed-complaints__2017-09.csv")
# rost <- read.csv("Data/new_data/roster__2018-03.csv")
# old_rost <- read.csv("Data/new_data/roster_1936-2017_2017-04.csv")
# salary <- read.csv("Data/new_data/salary_2002-2017_2017-09.csv")
# unit <- read.csv("Data/new_data/unit-history__2016-12.csv")
# trr <- read.csv("Data/new_data/TRR-main_2004-2016_2016-09.csv")
# trr_off <- read.csv("Data/new_data/TRR-officers_2004-2016_2016-09.csv")
# trr_sub <- read.csv("Data/new_data/TRR-subjects_2004-2016_2016-09.csv")
# settlements <- read.csv("Data/new_data/settlements.csv")
# 
# ##processing
# #joining the old roster to the new roster
# old_rost <- subset(old_rost, select=c("row_id",
# "gender","race","current_age",
# "current_unit","appointed_date",
# "resignation_date","first_name","last_name",
# "middle_initial","UID"))
# 
# rost <- subset(rost, select=c("row_id",
# "gender","race","current_age",
# "unit","appointed_date",
# "resignation_date","first_name","last_name",
# "middle_initial","UID"))
# colnames(rost)[5] <- "current_unit"
# 
# rost <- unique(rbind(rost, old_rost))
# 
# #treat 'complaints' as central file for complaints:
# #join information to that.
# #add complainant gender
# v <- aggregate(gender~cr_id, data=comp_comp,
# paste, collapse=";")
# complaints$comp_gender <- v[match(complaints$cr_id, 
# v$cr_id),]$gender
# v <- aggregate(race~cr_id, data=comp_comp,
# paste, collapse=";")
# complaints$comp_race <- v[match(complaints$cr_id, 
# v$cr_id),]$race
# #add victim info
# #add age
# comp_comp$age <- year(ymd(comp_comp$complaint_date))-comp_comp$birth_year
# #add victim gender
# v <- aggregate(age~cr_id, data=comp_comp,
# paste, collapse=";")
# complaints$comp_age <- v[match(complaints$cr_id, 
# v$cr_id),]$age
# v <- aggregate(gender~cr_id, data=comp_vic,
# paste, collapse=";")
# complaints$vic_gender <- v[match(complaints$cr_id, 
# v$cr_id),]$gender
# v <- aggregate(race~cr_id, data=comp_vic,
# paste, collapse=";")
# complaints$vic_race <- v[match(complaints$cr_id, 
# v$cr_id),]$race
# comp_vic$age <- year(ymd(comp_vic$complaint_date))-
# comp_vic$birth_year
# v <- aggregate(age~cr_id, data=comp_vic,
# paste, collapse=";")
# complaints$vic_age <- v[match(complaints$cr_id, 
# v$cr_id),]$age
# #don't need to add investigator info at this time.
# #add comp_witness info
# v <- aggregate(UID~cr_id, data=comp_wit,
# paste, collapse=";")
# complaints$wit_uid <- v[match(complaints$cr_id, 
# v$cr_id),]$UID
# #add whether complaint was upheld
# complaints$final_finding <- comp_acc[match(complaints$cr_id,
# comp_acc$cr_id),]$final_finding
# #flag for officer filed or not
# comp_acc$off <- comp_acc$cr_id%in%off_comp$cr_id
# complaints$off <- complaints$cr_id%in%off_comp$cr_id
# #treat prof as central file for officers
# prof$sdate <- year(ymd(prof$appointed_date))
# prof$edate <- year(ymd(prof$resignation_date))
# prof$edate[is.na(prof$edate)] <- 2018
# 
# ##break data into three chunks: pre-IPRA, post-IPRA, post McDonald case.
# comp_acc$date <- complaints[match(comp_acc$cr_id, 
# complaints$cr_id),]$complaint_date
# comp_acc$year <- year(ymd(comp_acc$date))
# train <- subset(comp_acc, year(ymd(comp_acc$date))<2008)
# test <- subset(comp_acc, year(ymd(comp_acc$date))>2007&
# year(ymd(comp_acc$date))<2015)
# test2 <- subset(comp_acc, year(ymd(comp_acc$date))>2014)
# 
# ##adjust for unit history
# #replace missing unit end dates with current date
# unit$unit_end_date <- as.character(unit$unit_end_date)
# unit$unit_end_date[unit$unit_end_date==""|is.na(unit$unit_end_date)] <-
# "2016-10-08"
# #
# v <- vector()
# for(i in 1:nrow(comp_acc))
# {
# if(nrow(subset(unit, UID==comp_acc$UID[i]&
# ymd(comp_acc$date[i])>ymd(unit_start_date)&
# ymd(comp_acc$date[i])<ymd(unit_end_date)))==0)
# {v[i] <- "NA"; next}
# if(nrow(subset(unit, UID==comp_acc$UID[i]&
# ymd(comp_acc$date[i])>ymd(unit_start_date)&
# ymd(comp_acc$date[i])<ymd(unit_end_date)))>1)
# {v[i] <- "ambiguous"; next}
# else
# {v[i] <- subset(unit, UID==comp_acc$UID[i]&
# ymd(comp_acc$date[i])>ymd(unit_start_date)&
# ymd(comp_acc$date[i])<ymd(unit_end_date))$unit}
# print(i)
# }
# comp_acc$unit <- v
# #get complaints per day numbers
# unit_adj <- data.frame(table(comp_acc$unit))
# unit$days <- as.numeric(ymd(unit$unit_end_date)-
# ymd(unit$unit_start_date))
# unit$days[unit$days<0] <- 1
# y <- aggregate(days~unit, data=unit, sum)
# unit_adj$pers <- y[match(unit_adj$Var1, y$unit),2]
# unit_adj$perday <- unit_adj$Freq/unit_adj$pers
# #build an "expected" number of complaints for each stint
# #then sum to provide the expectation over each time period
# unit$expected <- unit$days*unit_adj[match(unit$unit, unit_adj$Var1),]$perday
# #divide into time periods
# y <- aggregate(expected~UID, 
# data=subset(unit, year(ymd(unit_end_date))<2008),
# sum)
# prof$train_exp <- y[match(prof$UID, y$UID),2]
# y <- aggregate(expected~UID, 
# data=subset(unit, year(ymd(unit_end_date))>2007&
# year(ymd(unit_end_date))<2015),
# sum)
# prof$test_exp <- y[match(prof$UID, y$UID),2]
# y <- aggregate(expected~UID, 
# data=subset(unit, year(ymd(unit_end_date))>2014),
# sum)
# prof$test2_exp <- y[match(prof$UID, y$UID),2]
# 
# ##add social data
# v <- data.frame(table(unique(train[,c("UID","cr_id")])$UID))
# comp_acc$train <- v[match(comp_acc$UID,
# v$Var1),2]
# #need to remove duplicate rows
# v <- aggregate(train~cr_id, data=
# unique(comp_acc[,c("cr_id", "UID", "train")]), sum)
# comp_acc$cocomp <- v[match(comp_acc$cr_id, v$cr_id),2]
# v <- aggregate(cocomp~UID, data=comp_acc,
# sum)
# prof$cocomp <- v[match(prof$UID, v$UID),2]
# 
# v <- data.frame(table(unique(train[,c("UID","cr_id")])$UID))
# prof$train <- v[match(prof$UID, v$Var1),2]
# v <- data.frame(table(unique(test[,c("UID","cr_id")])$UID))
# prof$test <- v[match(prof$UID, v$Var1),2]
# v <- data.frame(table(unique(test2[,c("UID","cr_id")])$UID))
# prof$test2 <- v[match(prof$UID, v$Var1),2]
# prof$train[is.na(prof$train)] <- 0
# prof$test[is.na(prof$test)] <- 0
# prof$test2[is.na(prof$test2)] <- 0
# 
# prof$cocomp[is.na(prof$cocomp)] <- 0
# #officer-only complaints
# v <- data.frame(table(subset(train, off)$UID))
# prof$traino <- v[match(prof$UID, v$Var1),2]
# v <- data.frame(table(subset(test, off)$UID))
# prof$testo <- v[match(prof$UID, v$Var1),2]
# v <- data.frame(table(subset(test2, off)$UID))
# prof$test2o <- v[match(prof$UID, v$Var1),2]
# prof$traino[is.na(prof$train)] <- 0
# prof$testo[is.na(prof$test)] <- 0
# prof$test2o[is.na(prof$test2)] <- 0
# 
# #add number of years in training period
# prof$trainy <- as.numeric(ymd("2008-1-1")-ymd(prof$appointed_date))
# prof$trainy[prof$trainy>2920] <- 8*365
# prof$trainy[prof$trainy<0] <- NA
# prof$trainy[prof$trainy>(365*8)] <- 8
# v <- as.numeric(ymd("2008-1-1")-ymd(prof$resignation_date))
# v[(v<0)|is.na(v)] <- 0
# prof$trainy <- prof$trainy-v
# prof$trainy[prof$trainy<0] <- NA
# #add number of years in testing period
# prof$testy <- as.numeric(ymd("2015-1-1")-ymd(prof$appointed_date))
# prof$testy[prof$testy>2555] <- 7*365
# prof$testy[prof$testy<0] <- NA
# prof$testy[prof$testy>(365*7)] <- 7
# v <- as.numeric(ymd("2011-1-1")-ymd(prof$resignation_date))
# v[(v<0)|is.na(v)] <- 0
# prof$testy <- prof$testy-v
# prof$testy[prof$testy<0] <- NA
# #and 2nd testing period
# prof$test2y <- as.numeric(ymd("2018-3-1")-ymd(prof$appointed_date))
# prof$test2y[prof$test2y>1155] <- 1155
# prof$test2y[prof$test2y<0] <- NA
# prof$test2y[prof$test2y>(1155)] <- 1155
# v <- as.numeric(ymd("2018-3-1")-ymd(prof$resignation_date))
# v[(v<0)|is.na(v)] <- 0
# prof$test2y <- prof$test2y-v
# prof$test2y[prof$test2y<0] <- NA
# 
# #making complaint a rate stat
# prof$trainr <- prof$train/prof$trainy
# prof$testr <- prof$test/prof$testy
# prof$test2r <- prof$test2/prof$test2y
# 
# prof$trainr[is.infinite(prof$trainr)] <- 0
# prof$testr[is.infinite(prof$testr)] <- 0
# #now compare it to expected
# prof$train_vs <- prof$trainr/prof$train_exp
# prof$test_vs <- prof$testr/prof$test_exp
# prof$test2_vs <- prof$test2r/prof$test2_exp
# 
# prof$cocomp[is.na(prof$cocomp)] <- 0
# prof$cocomp <- prof$cocomp-prof$train
# 
# #building social networks
# 
# #first, an interaction matrix:
# mat <- matrix(0, nrow=length(unique(comp_acc$UID)),
# ncol=length(unique(comp_acc$UID)))
# v <- unique(comp_acc$UID)
# for(i in 1:length(unique(comp_acc$UID)))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(comp_acc, UID==v[i])$cr_id)
# mat[i,] <- (v%in%g$UID)
# mat[,i] <- (v%in%g$UID)
# print(i)
# }
# #fix some stuff
# diag(mat) <- 0
# g <- graph.adjacency(mat[1:100,1:100], mode="undirected")
# g <- simplify(g)
# V(g)$label <- v[1:100]
# V(g)$degree <- degree(g)
# #plot a chart
# plot(g, layout=layout.fruchterman.reingold(g))
# ##get node betweenness
# mat <- matrix(0, 
# nrow=length(unique(subset(comp_acc, year<2008)$UID)),
# ncol=length(unique(subset(comp_acc, year<2008)$UID)))
# v <- unique(subset(comp_acc, year<2008)$UID)
# dates <- vector()
# for(i in 1:length(unique(subset(comp_acc, year<2008)$UID)))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(comp_acc, year<2008&UID==v[i])$cr_id)
# mat[i,] <- (v%in%g$UID)
# mat[,i] <- (v%in%g$UID)
# print(i)
# if(nrow(g)>0)
# {dates <- append(dates, unique(g$date))}
# #print(length(unique(g$date)))
# }
# #fix some stuff
# diag(mat) <- 0
# g <- graph.adjacency(mat, mode="undirected")
# g <- simplify(g)
# V(g)$label <- v
# V(g)$name <- v
# V(g)$degree <- degree(g)
# a <- get.edgelist(g)
# a <- as.data.frame(a)
# a <- a[!is.na(a$V1)&!is.na(a$V2),]
# write.csv(a, "SocialNetwork_before2008.csv")
# #y <- betweenness(g)
# z <- data.frame(UID=v, bet=y)
# rost$bet <- z[match(rost$UID, z$UID),2]
# 
# plot(prof$trainr+.1, prof$testr+.1, log="xy", 
# xlab="Training Complaint Rate", ylab="Test Complaint Rate",
# pch=16, col=rgb(0,0,0,.4))
# 
# plot(prof$cocomp+.1, prof$testr+.1, log="xy", 
# xlab="Co-complaint rate", ylab="Test Complaint Rate",
# pch=16, col=rgb(0,0,0,.4))
# 
# cor(prof$trainr, prof$testr, use="complete.obs")
# cor(prof$cocomp, prof$testr, use="complete.obs")
# cor(prof$trainr, prof$testr, use="complete.obs", method="spearman")
# cor(prof$cocomp, prof$testr, use="complete.obs", method="spearman")
# #-size of main network
# count_max_cliques(g)
# #-total number of officers
# nrow(prof)
# #-total number of complaints
# sum(prof$train)+sum(prof$test)+sum(prof$test2)
# nrow(unique(comp_acc[,c("UID","cr_id")]))
# 
# #extract "contagious" ones
# v <- subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%
# subset(prof, cocomp>100)$UID)$cr_id)
# 
# mean(subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007&year<2014)$UID))$test2r,
# na.rm=T)
# 
# mean(subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007)$UID)&
# !is.infinite(testr))$test2r,
# na.rm=T)
# 
# mean(subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007)$UID)&
# !is.infinite(testr))$testr,
# na.rm=T)
# 
# prop.table(table(subset(prof, trainr<1&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007)$UID)&
# !is.infinite(testr))$current_rank))
# 
# prop.table(table(subset(prof, trainr<1&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007)$UID)&
# !is.infinite(testr))$current_rank))
# 
# mean(subset(prof, trainr<1&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)$UID)&
# !is.infinite(test2r))$test2r,
# na.rm=T)
# table(is.na(subset(prof, trainr<1&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)))$test2r))
# 
# barplot(c(0.1825, .0365), 
# names.arg=c("Listed on complaint with high-complaint officer",
# "Not listed on complaint with high-complaint officer"),
# col=c("red", "blue"),
# ylab="Later complaint rate (per year)")
# 
# #does it work the other way too?
# v <- subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%
# subset(rost, cocomp<10)$UID)$cr_id)
# 
# mean(subset(prof, trainr>.0004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007&year<2014)$UID))$test2r,
# na.rm=T)
# 
# nrow(subset(prof, trainr>.0004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007&year<2014)$UID)))
# 
# mean(subset(prof, trainr>.0004&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)$UID)&
# !is.infinite(test2r))$test2r,
# na.rm=T)
# 
# nrow(subset(prof, trainr>.0004&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)$UID)&
# !is.infinite(test2r)))
# #not really.
# #how many additional uses of force
# a <- subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007&year<2014)$UID))
# nrow(subset(trr, UID%in%a$UID&year(ymd(trr_date))>2007))/
# sum(a$testy, na.rm=T)
# 
# a <- subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)$UID))
# nrow(subset(trr, UID%in%a$UID&year(ymd(trr_date))>2007))/
# sum(a$testy, na.rm=T)
# 
# #how many additional shootings
# a <- subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007&year<2014)$UID))
# nrow(subset(trr_fire, UID%in%a$UID&year(ymd(trr_date))>2007))/
# sum(a$testy, na.rm=T)
# 
# a <- subset(prof, trainr<.004&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007&year<2014)$UID))
# nrow(subset(trr_fire, UID%in%a$UID&year(ymd(trr_date))>2007))/
# sum(a$testy, na.rm=T)
# 
# #
# ##
# mean(subset(rost, trainr<1&
# (!UID%in%subset(v, year<2006)$UID)&
# (UID%in%subset(v, year>2005&year<2012)$UID))$trainr,
# na.rm=T)
# 
# mean(subset(rost, trainr<1&
# (!UID%in%subset(v, year<2006)$UID)&
# (!UID%in%subset(v, year>2005&year<2012)$UID)&
# !is.infinite(test2r))$trainr,
# na.rm=T)
# 
# ##preliminary TRR/shooting prediction
# trr$race <- trr_sub[match(trr$trr_id, trr_sub$trr_id),]$race
# trr$gender <- trr_sub[match(trr$trr_id, trr_sub$trr_id),]$gender
# trr$age <- trr_sub[match(trr$trr_id, trr_sub$trr_id),]$age
# trr$UID <- trr_off[match(trr$trr_id, trr_off$trr_id),]$UID
# 
# trr_fire <- subset(trr, total_number_of_shots>0)
# trr_fire$race <- trr_sub[match(trr_fire$trr_id, trr_sub$trr_id),]$race
# trr_fire$age <- trr_sub[match(trr_fire$trr_id, trr_sub$trr_id),]$age
# trr_fire$UID <- trr_off[match(trr_fire$trr_id, trr_off$trr_id),]$UID
# 
# #characterize histogram
# #trr_fire <- subset(trr_fire, year(ymd(trr_date))<2016)
# trr_table <- data.frame(table(trr_fire$UID))
# sum(subset(trr_table, Freq>1)$Freq)
# sum(trr_table$Freq)
# 
# v <- data.frame(table(comp_acc$UID))
# trr_table$comp <- v[match(trr_table$Var1, v$Var1),2]
# trr_table$comp[is.na(trr_table$comp)] <- 0
# 
# mean(subset(trr_table, Freq>1)$comp)
# mean(subset(trr_table, Freq==1)$comp)
# 
# barplot(c(mean(subset(trr_table, Freq>1)$comp),
# mean(subset(trr_table, Freq==1)$comp),
# mean(v$Freq)), names.arg=c("More than 1 shooting",
# "1 shooting", "0 shootings"), 
# col=c("red", "blue", "grey"),
# ylab="Lifetime Complaint Total")
# 
# #one officer particularly interesting:
# "109870"
# subset(comp_acc, UID=="109870")
# #
# v <- vector()
# z <- vector()
# for(i in 1:nrow(trr_fire))
# {
# g <- unique(subset(comp_acc, UID==trr_fire$UID[i],
# select=c("UID","cr_id","date","cocomp")))
# v[i] <- nrow(subset(g, 
# ymd(date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(date)<ymd(trr_fire$trr_date[i])))
# z[i] <- sum(unique(g)$cocomp, na.rm=T)
# print(i)
# }
# trr_fire$precomp <- v
# 
# y <- vector()
# a <- vector()
# for(i in 1:1000)
# {
# g <- unique(subset(comp_acc, UID==sample(unique(comp_acc$UID), 1),
# select=c("UID","cr_id","date","cocomp")))
# y[i] <- nrow(subset(g, 
# ymd(date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(date)<ymd(trr_fire$trr_date[i])))
# a[i] <- sum(unique(g)$UID, na.rm=T)
# print(i)
# }
# 
# rand_samp <- data.frame(off_uid=a, complaints_before=y)
# rand_samp$dt <- (trr_fire$trr_date)
# 
# y <- vector()
# for(i in 1:1000)
# {
# g <- unique(subset(trr_fire, UID==rand_samp$off_uid[i],
# select=c("UID","trr_id","trr_date")))
# y[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# rand_samp$shootings_before <- y
# 
# y <- vector()
# for(i in 1:1000)
# {
# g <- unique(subset(trr, UID==rand_samp$off_uid[i],
# select=c("UID","trr_id","trr_date")))
# if(nrow(g)==0) {next}
# y[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# y[is.na(y)] <- 0
# rand_samp$useofforce_before <- y
# #
# write.csv(rand_samp, "RandomSample.csv")
# #
# v <- vector()
# z <- vector()
# for(i in 1:nrow(trr_fire))
# {
# g <- unique(subset(trr, UID==trr_fire$UID[i],
# select=c("UID","trr_id","trr_date")))
# v[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# trr_fire$pretrr <- v
# 
# y <- vector()
# a <- vector()
# for(i in 1:1000)
# {
# g <- unique(subset(trr_fire, UID==sample(unique(comp_acc$UID), 1),
# select=c("UID","trr_id","trr_date")))
# y[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# 
# y <- vector()
# a <- vector()
# for(i in 1:1000)
# {
# g <- unique(subset(trr, UID==sample(unique(comp_acc$UID), 1),
# select=c("UID","trr_id","trr_date")))
# y[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# 
# 
# #Odds ratio of 1.6-2
# (4967*78)/(33*948)
# 
# table(v>1)
# table(y>1)
# 
# fisher.test(matrix(c(397,1026,237,1000), ncol=2))
# 
# #make a chart
# png("Charts/Complaints_vs_Fires.png", height=1080, width=1080)
# par(cex=2.4)
# barplot(c(mean(v), mean(y)), 
# names.arg=c("Officers who fired", "Randomly selected officers"),
# col=c("red", "blue"), ylab="Average number of complaints in the last year")
# dev.off()
# 
# #two top ten officers for shootings listed on the same complaint!
# subset(comp_acc, UID=="112549")$cr_id%in%
# subset(comp_acc, UID=="131946")$cr_id
# subset(comp_acc, cr_id=="1053275")
# 
# v <- vector()
# for(i in 1:nrow(trr_table))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(comp_acc, UID==trr_table$Var1[i])$cr_id&
# !UID==trr_table$Var1[i])
# v[i] <- sum(g$UID%in%trr_table$Var1)>0
# print(i)
# }
# sum(v)
# 
# ##follow-up work
# prof$overall_complaint_rate <- 
# rowSums(prof[,c("train","test","test2")], na.rm=T)/
# rowSums(prof[,c("trainy","testy","test2")], na.rm=T)
# #drop infinite officers
# prof <- subset(prof, !is.infinite(overall_complaint_rate))
# write.csv(prof, "Profiles_withAmts_Comprates.csv")
# 
# #complaints vs. random complaints
# subset(comp_acc, 
# 
# ##do uses of force predict further use of force
# trr$UID <- trr_off[match(trr$trr_id, trr_off$trr_id),]$UID
# 
# v <- vector()
# for(i in 1:nrow(trr_fire))
# {
# g <- subset(trr, UID==trr_fire$UID[i])
# v[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# trr_fire$pretrr <- v
# 
# y <- vector()
# for(i in 1:1000)
# {
# g <- subset(trr, UID==sample(comp_acc$UID, 1))
# y[i] <- nrow(subset(g, 
# ymd(trr_date)>(ymd(trr_fire$trr_date[i])-days(365))&
# ymd(trr_date)<ymd(trr_fire$trr_date[i])))
# print(i)
# }
# 
# ##
# test <- subset(trr, UID%in%subset(trr_table, Freq>1)$Var1)
# v <- subset(trr, !UID%in%subset(trr_table, Freq>1)$Var1)
# fisher.test(matrix(c(23, 432, 436, 5208), nrow=2))
# 
# prop.table(table(test$gender))
# prop.table(table(v$gender))
# 
# mean(test$age, na.rm=T)
# mean(v$age, na.rm=T)
# 
# ##bring in settlement data
# settlements$fname <- paste(settlements$first_name, settlements$last_name)
# settlements$amt <- as.numeric(as.character(
# gsub(",", "", gsub("\\$", "", settlements$settlement))))
# set_tot <- aggregate(amt~fname+oid, data=settlements, sum)
# 
# prof$fname <- paste(prof$first_name, prof$last_name)
# trr_table$fname <- prof[match(trr_table$Var1, prof$UID), ]$fname
# trr_table$amt <- set_tot[match(trr_table$fname, set_tot$fname),]$amt
# prof$amt <- set_tot[match(prof$fname, set_tot$fname),]$amt
# trr_table$amt[is.na(trr_table$amt)] <- 0
# prof$amt[is.na(prof$amt)] <- 0
# 
# ##
# settlements$fname <- paste(settlements$first_name, settlements$last_name)
# settlements$amt <- as.numeric(as.character(
# gsub(",", "", gsub("\\$", "", settlements$settlement))))
# set_tot <- aggregate(amt~fname+oid, 
# data=subset(settlements,!grepl("Excessive", complaint)), sum)
# 
# rost$fname <- paste(rost$first_name, rost$last_name)
# trr_table$fname <- rost[match(trr_table$Var1, rost$UID), ]$fname
# trr_table$amt <- set_tot[match(trr_table$fname, set_tot$fname),]$amt
# rost$amt <- set_tot[match(rost$fname, set_tot$fname),]$amt
# trr_table$amt[is.na(trr_table$amt)] <- 0
# rost$amt[is.na(rost$amt)] <- 0
# 
# mean(trr_table$amt)
# mean(rost$amt)
# 
# ##magnet effect
# a <- subset(prof, cocomp>100)
# mean(
# subset(prof, UID%in%subset(comp_acc, cr_id%in%
# subset(comp_acc, UID%in%a$UID)$cr_id)$UID&!
# UID%in%a$UID)$trainr
# , na.rm=T)
# mean(prof$trainr, na.rm=T)
# 
# #add TRRs to profile
# b <- data.frame(table(trr$UID))
# prof$trr <- b[match(prof$UID, b$Var1),2]
# prof$trr[is.na(prof$trr)] <- 0
# mean(
# subset(prof, UID%in%subset(comp_acc, cr_id%in%
# subset(comp_acc, UID%in%a$UID)$cr_id)$UID&!
# UID%in%a$UID)$trr
# , na.rm=T)
# mean(prof$trr, na.rm=T)
# 
# mean(subset(prof, UID%in%subset(unit, unit=="312")$UID)$trr)
# mean(subset(prof, UID%in%subset(unit, unit=="312")$UID&!UID%in%a$UID)$trr)
# 
# out <- data.frame(prop.table(table(a$current_unit_description)))
# out$department <- data.frame(prop.table(table(prof$current_unit_description)))[,2]
# colnames(out) <- c("unit_desc", "Frequency_in_CentralOfficers",
# "Departmental_Average")
# write.csv(out, "GangUnitDistribution.csv")
# 
# out <- subset(prof, !UID%in%a$UID&
# grepl("GANG ENFORCE", current_unit_description))
# write.csv(out, "NonCentralOfficers_whotransfer_to_gangunits.csv")
# 
# ##
# b <- subset(prof, UID%in%subset(comp_acc, cr_id%in%
# subset(comp_acc, UID%in%a$UID)$cr_id)$UID&!
# UID%in%a$UID)
# write.csv(subset(b, !is.infinite(overall_complaint_rate)),
# "Complainants_listed_with_centralofficers.csv")
# 
# v <- prof[sample(1:nrow(prof), 2000),]
# v <- subset(v, !is.infinite(overall_complaint_rate))
# mean(v$overall_complaint_rate, na.rm=T)
# write.csv(v, "RandomOfficers_complaintrates.csv")
# 
# ##generate inflection point data for Mariam/Moiz
# #extract "contagious" ones
# v <- subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%
# subset(prof, cocomp>100)$UID)$cr_id)
# 
# contacted <- subset(prof, !UID%in%subset(prof, cocomp>100)$UID&
# trainr<.0004&
# (!UID%in%subset(v, year<2008)$UID)&
# (UID%in%subset(v, year>2007)$UID))
# 
# non_con <- subset(prof, !UID%in%subset(prof, cocomp>100)$UID&
# trainr<.0004&!is.na(trainr)&
# (!UID%in%subset(v, year<2008)$UID)&
# (!UID%in%subset(v, year>2007)$UID)&
# !is.infinite(testr))
# 
# a <- data.frame(table(subset(trr, ymd(trr_date)>
# ymd("2008-1-1"))$UID))
# contacted$trr <- a[match(contacted$UID, a$Var1),2]
# non_con$trr <- a[match(non_con$UID, a$Var1),2]
# 
# a <- data.frame(table(subset(trr, ymd(trr_date)>
# ymd("2008-1-1"))$UID))
# contacted$trr <- a[match(contacted$UID, a$Var1),2]
# non_con$trr <- a[match(non_con$UID, a$Var1),2]
# contacted$trr[is.na(contacted$trr)] <- 0
# non_con$trr[is.na(non_con$trr)] <- 0
# 
# a <- data.frame(table(subset(trr, ymd(trr_date)>
# ymd("2008-1-1")&firearm_used=="1")$UID))
# contacted$shoot <- a[match(contacted$UID, a$Var1),2]
# non_con$shoot <- a[match(non_con$UID, a$Var1),2]
# contacted$shoot[is.na(contacted$shoot)] <- 0
# non_con$shoot[is.na(non_con$shoot)] <- 0
# 
# contacted$post_rate <- (contacted$test+contacted$test2)/
# (contacted$testy+contacted$test2y)
# write.csv(contacted, "Contacted_officers.csv")
# write.csv(non_con, "NonCon_officers.csv")
# 
# ##generate networks for Moiz
# mat <- matrix(0, 
# nrow=length(unique(subset(comp_acc, year>2007&year<2015)$UID)),
# ncol=length(unique(subset(comp_acc, year>2007&year<2015)$UID)))
# v <- unique(subset(comp_acc, year>2007&year<2015)$UID)
# dates <- vector()
# for(i in 1:length(unique(subset(comp_acc, year>2007&year<2015)$UID)))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(comp_acc, year>2007&year<2015&UID==v[i])$cr_id)
# mat[i,] <- (v%in%g$UID)
# mat[,i] <- (v%in%g$UID)
# print(i)
# if(nrow(g)>0)
# {dates <- append(dates, unique(g$date))}
# #print(length(unique(g$date)))
# }
# #fix some stuff
# diag(mat) <- 0
# g <- graph.adjacency(mat, mode="undirected")
# g <- simplify(g)
# V(g)$label <- v
# V(g)$name <- v
# V(g)$degree <- degree(g)
# a <- get.edgelist(g)
# a <- as.data.frame(a)
# a <- a[!is.na(a$V1)&!is.na(a$V2),]
# write.csv(a, "SocialNetwork_from2008-2014.csv")
# 
# ##get graph from 2015 on
# mat <- matrix(0, 
# nrow=length(unique(subset(comp_acc, year>2014)$UID)),
# ncol=length(unique(subset(comp_acc, year>2014)$UID)))
# v <- unique(subset(comp_acc, year>2014)$UID)
# dates <- vector()
# for(i in 1:length(unique(subset(comp_acc, year>2014)$UID)))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(comp_acc, year>2014&UID==v[i])$cr_id)
# mat[i,] <- (v%in%g$UID)
# mat[,i] <- (v%in%g$UID)
# print(i)
# if(nrow(g)>0)
# {dates <- append(dates, unique(g$date))}
# #print(length(unique(g$date)))
# }
# #fix some stuff
# diag(mat) <- 0
# g <- graph.adjacency(mat, mode="undirected")
# g <- simplify(g)
# V(g)$label <- v
# V(g)$name <- v
# V(g)$degree <- degree(g)
# a <- get.edgelist(g)
# a <- as.data.frame(a)
# a <- a[!is.na(a$V1)&!is.na(a$V2),]
# write.csv(a, "SocialNetwork_from2015on.csv")
# 
# 
# ##extract network around contagious officers for use
# ##get graph from 2015 on
# x <- subset(prof, cocomp>100)$UID
# y <- subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%x)$cr_id)
# mat <- matrix(0, 
# nrow=length(unique(subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%x)$cr_id)$UID)),
# ncol=length(unique(subset(comp_acc, 
# cr_id%in%subset(comp_acc, UID%in%x)$cr_id)$UID)))
# v <- unique(subset(comp_acc,
# cr_id%in%subset(comp_acc, UID%in%x)$cr_id)$UID)
# dates <- vector()
# for(i in 1:length(unique(y$UID)))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(y, UID==v[i])$cr_id)
# mat[i,] <- (v%in%g$UID)
# mat[,i] <- (v%in%g$UID)
# print(i)
# if(nrow(g)>0)
# {dates <- append(dates, unique(g$date))}
# }
# #fix some stuff
# diag(mat) <- 0
# g <- graph.adjacency(mat, mode="undirected")
# g <- simplify(g)
# V(g)$label <- v
# V(g)$name <- v
# V(g)$degree <- degree(g)
# a <- get.edgelist(g)
# a <- as.data.frame(a)
# a <- a[!is.na(a$V1)&!is.na(a$V2),]
# #now to add a date column...
# z <- vector()
# for(i in 1:nrow(a))
# {
# b <- subset(y, UID==a$V1[i])$cr_id
# z[i] <- as.character(min(ymd(subset(y, cr_id%in%b&UID==a$V2[i])$date)))
# print(i)
# }
# colnames(a) <- c("Source", "Target", "Date")
# a$start_date <- z
# a$end_date <- "2018-1-1"
# z <- vector()
# for(i in 1:nrow(a))
# {
# b <- subset(trr, UID==a$Source[i])
# z[i] <- nrow(b)
# print(i)
# }
# a$trr <- z
# write.csv(a, "SocialNetwork_forChart.csv",
# row.names=F)
# 
# ##need to limit the viz to just one particular cluster
# ##picking Watts
# "126443"
# #or Finnigan
# "112944"
# y <- subset(comp_acc, UID%in%subset(comp_acc, UID%in%
# subset(comp_acc, cr_id%in%
# subset(comp_acc, UID=="132263")$cr_id)$UID)$UID)
# 
# y <- 
# subset(comp_acc, cr_id%in%subset(comp_acc, UID%in%
# subset(comp_acc, cr_id%in%subset(comp_acc, UID=="132263")$cr_id)$UID)$cr_id)
# y <- subset(y, !is.na(UID))
# b <- subset(comp_acc, UID%in%subset(comp_acc, UID%in%
# subset(comp_acc, cr_id%in%
# subset(comp_acc, UID=="132263")$cr_id)$UID)$UID)
# y <- subset(y, !UID%in%b$UID)
# y <- y[1:500,]
# y <- rbind(y, b)
# y <- rbind(comp_acc[sample(1:nrow(comp_acc), 200),],y)
# length(unique(y$UID))
# 
# mat <- data.frame(source=NA,
# target=NA, date=NA)
# v <- unique(y$UID)
# 
# for(i in 1:length(v))
# {
# g <- subset(comp_acc, cr_id%in%
# subset(y, UID==v[i])$cr_id)
# if(nrow(g)==0) {next}
# mat <- rbind(mat, data.frame(source=v[i],
# target=g$UID, date=g$date))
# print(i)
# }
# #fix some stuff
# a <- na.omit(mat)
# colnames(a) <- c("Source", "Target", "Date")
# a$start_date <- a$Date
# a$end_date <- "2018-1-1"
# a <- subset(a, !Source==Target)
# a$name <- prof[match(a$Source, prof$UID),]$fname
# write.csv(a, "SocialNetwork_forChart.csv",
# row.names=F)
# 
# #now need to generate dynamic characteristics 
# #for each node & timeframe
# z <- vector()
# x <- vector()
# for(i in 1:length(unique(y$UID)))
# {
# print(i)
# for(j in 2000:2017)
# {
# #extract use of force value
# g <- subset(trr, UID==unique(y$UID)[i]&
# ymd(trr_date)<ymd(paste(j+1, "01-01", sep="-")))
# z <- append(z, 
# paste(paste("<[", paste(j, "01-01,", sep="-"), sep=""),
# paste(paste(paste(j+1, "01-01", sep="-"), "", sep="")),
# paste(paste(",", nrow(g), sep=""), "]>", sep=""), sep="")
# )
# g <- subset(comp_acc, UID==unique(y$UID)[i]&
# ymd(date)<ymd(paste(j+1, "01-01", sep="-")))
# x <- append(x, 
# paste(paste("<[", paste(j, "01-01,", sep="-"), sep=""),
# paste(paste(paste(j+1, "01-01", sep="-"), "", sep="")),
# paste(paste(",", nrow(g), sep=""), "]>", sep=""), sep="")
# )
# }
# }
# a <- data.frame(Id=rep(unique(y$UID), 18), trr=z, comp=x)
# b <- aggregate(sort(trr)~Id, a, paste, collapse=";")
# b$comp <- aggregate(sort(comp)~Id, a, paste, collapse=";")[,2]
# colnames(b)[2] <- "trr"
# b$name <- prof[match(b$Id, prof$UID),]$fname
# write.csv(b, "EdgeTRR.csv", row.names=F)
# 