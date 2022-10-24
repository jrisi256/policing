library(here)
source(here("functions.R"))

unit_shift_net <- readRDS(here("create-networks", "output", "unit_shift_net_bp.rds"))
stop_net <- readRDS(here("create-networks", "output", "stop_net_bp.rds"))
arrest_net <- readRDS(here("create-networks", "output", "arrest_net_bp.rds"))

officers <-
    my_read_csv(here("create-networks", "input", "active_officers_ba.csv")) %>%
    mutate(age = 2015 - birth_year,
           years_exp = (ymd("2015-12-01") - appointed_month) / dyears(1)) %>%
    select(-birth_year, -appointed_month)

# take an igraph object
# turn it into an adjacency matrix (to capture 0 edges)
# turn it into a data frame
# turn it into a long data frame to prepare for modelling
Transform_Into_Complete_Edgelist <- function(igraph_obj) {
    
    igraph_obj %>%
        as_adjacency_matrix(sparse = F, attr = "weight") %>%
        as.data.frame() %>%
        mutate(row_id = rownames(.)) %>%
        pivot_longer(cols = -row_id,
                     names_to = "col_id",
                     values_to = "weight") %>%
        mutate(row_id = as.character(row_id),
               col_id = as.character(col_id)) %>%
        filter(row_id != col_id)
}

AddRace <- function(df) {
    
    df %>%
        mutate(white_white = if_else(officer_race.x == "officer_white" & officer_race.y == "officer_white",
                                     1, 0),
               black_black = if_else(officer_race.x == "officer_black" & officer_race.y == "officer_black",
                                     1, 0),
               hisp_hisp = if_else(officer_race.x == "officer_hisp" & officer_race.y == "officer_hisp",
                                   1, 0),
               white_black = if_else((officer_race.x == "officer_black" & officer_race.y == "officer_white") |
                                         (officer_race.x == "officer_white" & officer_race.y == "officer_black"),
                                     1, 0),
               white_hisp = if_else((officer_race.x == "officer_white" & officer_race.y == "officer_hisp") |
                                        (officer_race.x == "officer_hisp" & officer_race.y == "officer_white"),
                                    1, 0),
               black_hisp = if_else((officer_race.x == "officer_black" & officer_race.y == "officer_hisp") |
                                        (officer_race.x == "officer_hisp" & officer_race.y == "officer_black"),
                                    1, 0))
}

AddSex <- function(df) {
    
    df %>%
        mutate(male_male = if_else(officer_gender.x == "MALE" & officer_gender.y == "MALE",
                                   1, 0)) %>%
        mutate(female_female = if_else(officer_gender.x == "FEMALE" & officer_gender.y == "FEMALE",
                                       1, 0)) %>%
        mutate(male_female = if_else((officer_gender.x == "MALE" & officer_gender.y == "FEMALE") |
                                         (officer_gender.x == "FEMALE" & officer_gender.y == "MALE"),
                                     1, 0))
}

shifts_df <-
    unit_shift_net %>%
    Transform_Into_Complete_Edgelist() %>%
    rowwise() %>%
    mutate(edge_id = paste0(max(row_id, col_id), "_", min(row_id, col_id))) %>%
    distinct(edge_id, .keep_all = T) %>%
    left_join(officers, by = c("row_id" = "officer_id")) %>%
    left_join(officers, by = c("col_id" = "officer_id")) %>%
    AddRace() %>%
    AddSex() %>%
    mutate(exp_diff = abs(years_exp.x - years_exp.y))

stops_df <-
    stop_net %>%
    Transform_Into_Complete_Edgelist() %>%
    rowwise() %>%
    mutate(edge_id = paste0(max(row_id, col_id), "_", min(row_id, col_id))) %>%
    distinct(edge_id, .keep_all = T) %>%
    left_join(officers, by = c("row_id" = "officer_id")) %>%
    left_join(officers, by = c("col_id" = "officer_id")) %>%
    AddRace() %>%
    AddSex() %>%
    mutate(exp_diff = abs(years_exp.x - years_exp.y)) %>%
    left_join(select(shifts_df, edge_id, weight), by = "edge_id") %>%
    mutate(nr_shifts = if_else(is.na(weight.y), 0, weight.y))

arrests_df <-
    arrest_net %>%
    Transform_Into_Complete_Edgelist() %>%
    rowwise() %>%
    mutate(edge_id = paste0(max(row_id, col_id), "_", min(row_id, col_id))) %>%
    distinct(edge_id, .keep_all = T) %>%
    left_join(officers, by = c("col_id" = "officer_id")) %>%
    left_join(officers, by = c("row_id" = "officer_id")) %>%
    AddRace() %>%
    AddSex() %>%
    mutate(exp_diff = abs(years_exp.x - years_exp.y)) %>%
    left_join(select(shifts_df, edge_id, weight), by = "edge_id") %>%
    mutate(nr_shifts = if_else(is.na(weight.y), 0, weight.y))

saveRDS(shifts_df, file = "shifts_df.rds")
saveRDS(stops_df, file = "stops_df.rds")
saveRDS(arrests_df, file = "arrests_df.rds")

ols_shifts <- lm(weight ~ white_black + white_hisp + black_black + black_hisp +
                     hisp_hisp + male_female + female_female + years_exp.x +
                     years_exp.y + exp_diff,
                 data = shifts_df)

ols_stops <- lm(weight.x ~ white_black + white_hisp + black_black + black_hisp +
                    hisp_hisp + male_female + female_female + years_exp.x +
                    years_exp.y + exp_diff + nr_shifts,
                data = stops_df)

ols_arrests <- lm(weight.x ~ white_black + white_hisp + black_black + black_hisp +
                      hisp_hisp + male_female + female_female + years_exp.x +
                      years_exp.y + exp_diff + nr_shifts,
                  data = arrests_df)

saveRDS(ols_shifts, file = "ols_shifts.rds")
saveRDS(ols_stops, file = "ols_stops.rds")
saveRDS(ols_arrests, file = "ols_arrests.rds")
