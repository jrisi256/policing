a <-
    officerAssignment %>%
    mutate(floor_start = floor_date(start_time, unit = "hour"),
           floor_end = ceiling_date(end_time, unit = "hour")) %>%
    setDT()

b <- stops %>% mutate(floor_time = floor_date(time, unit = "hour")) %>% setDT()
join <-
    b[a, on = .(officer_id, floor_time >= floor_start, floor_time <= floor_end)] %>%
    tibble() %>%
    mutate(final_id = paste0(s_id, " ", oa_id))