library(here)
library(dplyr)
library(readr)

complaintsAccusedTotal <-
    read_csv(here("unified_data", "full-output", "complaints", "complaints-accused.csv"),
             col_types = cols(recc_finding = "c", recc_outcome = "c", cr_id = "c"))
table(is.na(complaintsAccusedTotal$UID))["FALSE"] / nrow(complaintsAccusedTotal)

complaintsAccused6799 <-
    read_csv(here("unified_data", "full-output", "complaints", "complaints-accused_1967-1999_2016-12.csv"),
             col_types = cols(rank_no = "c", cr_id = "c"))
table(is.na(complaintsAccused6799$UID))["FALSE"] / nrow(complaintsAccused6799)

complaintsAccused0016 <-
    read_csv(here("unified_data", "full-output", "complaints", "complaints-accused_2000-2016_2016-11.csv"),
             col_types = cols(cr_id = "c"))
table(is.na(complaintsAccused0016$UID))["FALSE"] / nrow(complaintsAccused0016)

complaintsAccused0018 <-
    read_csv(here("unified_data", "full-output", "complaints", "complaints-accused_2000-2018_2018-03.csv"),
             col_types = cols(cr_id = "c"))
table(is.na(complaintsAccused0018$UID))["FALSE"] / nrow(complaintsAccused0018)

# Complaints from 1967 - 1999 not included in total because officer ID is missing
antij6799 <- anti_join(complaintsAccused6799, complaintsAccusedTotal, by = "cr_id")
table(is.na(antij6799$UID))

# No complaints from 2000 - 2016 are missing from the total
antij0016 <- anti_join(complaintsAccused0016, complaintsAccusedTotal, by = "cr_id")

# Complaints from 2000 - 2018 not included in total because officer ID is missing
antij0018 <- anti_join(complaintsAccused0018, complaintsAccusedTotal, by = "cr_id")
table(is.na(antij0018$UID))

# Cops and complaints match which is good
complaintsAccused6799Fltr <- complaintsAccused6799 %>% filter(!is.na(UID))
complaintsAccused0018Fltr <- complaintsAccused0018 %>% filter(!is.na(UID))

cops6799 <-
    left_join(complaintsAccused6799Fltr, select(complaintsAccusedTotal, cr_id, UID),
              by = c("cr_id", "UID"))
nrow(cops6799) == nrow(complaintsAccused6799Fltr)

cops0016 <-
    left_join(complaintsAccused0016, select(complaintsAccusedTotal, cr_id, UID),
              by = c("cr_id", "UID"))
nrow(cops0016) == nrow(complaintsAccused0016)

cops0018 <-
    left_join(complaintsAccused0018Fltr, select(complaintsAccusedTotal, cr_id, UID),
              by = c("cr_id", "UID"))
nrow(cops0018) == nrow(complaintsAccused0018Fltr)

# There are complaints from the 2016 data set not in the 2018 data set. The only
# reason I can think of is that these complaints don't have a newer 2018 entry
# which has more columns.
aj0016_0018 <-
    anti_join(complaintsAccused0016, complaintsAccused0018Fltr, by = "cr_id")

# There are some complaints in the 2018 data set not in the 2016 data set which
# makes sense because those complaints could have occurred after 2016.
aj0018_0016 <-
    anti_join(complaintsAccused0018Fltr, complaintsAccused0016, by = "cr_id")

# There are also some complaints in the 2016 and 2018 records which have different
# cop IDs. Some of the cr_id + UID pairs are used from 2016 while others are
# used from 2018 in the total file.

copDiff0016_0018 <-
    inner_join(select(complaintsAccused0016, cr_id, UID),
               select(complaintsAccused0018Fltr, cr_id, UID),
               by = "cr_id") %>%
    group_by(cr_id, UID.x) %>%
    filter(all(UID.x != UID.y)) %>%
    distinct(cr_id, UID.x)

copDiff0018_0016 <-
    inner_join(select(complaintsAccused0016, cr_id, UID),
               select(complaintsAccused0018Fltr, cr_id, UID),
               by = "cr_id") %>%
    group_by(cr_id, UID.y) %>%
    filter(all(UID.y != UID.x)) %>%
    distinct(cr_id, UID.y)

copDiff <- full_join(copDiff0016_0018, copDiff0018_0016, by = c("cr_id"))

copDiffTotal <- filter(complaintsAccusedTotal, cr_id %in% unique(copDiff$cr_id))
