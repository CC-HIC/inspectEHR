ctn <- connect(sqlite_file = "~/_data/hic/public/full_synthetic_db.sqlite3")
core <- make_core(ctn)
ref <- make_reference(ctn)

hr <- extract(core, "NIHR_HIC_ICU_0108")

episodes <- characterise_episodes(ctn)
ve_episodes <- verify_episodes(episodes)

hrv <- verify_events(hr, ve_episodes)

x <- hrv
reference_tbl <- ref

name_check <- dplyr::intersect(
    names(x),
    c("out_of_bounds", "range_error", "duplicate")
  )
  if (length(name_check) != 3) {
    rlang::abort("You must supply a dataframe to `x` that contains columns with
                 names `out_of_bounds`, `range_error` and `duplicate`")
  }
  
  base_events <- x %>%
    filter(
      .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
      .data$range_error == 0L | is.na(.data$range_error),
      .data$duplicate == 0L | is.na(.data$duplicate)
    ) %>%
    mutate(date = lubridate::as_date(datetime)) %>%
    group_by(site, date) %>%
    summarise(event_count = n_distinct(event_id))
  
  base_calendar <- reference_tbl %>%
    group_by(.data$site) %>%
    summarise(
      start = lubridate::as_date(
        lubridate::floor_date(min(.data$start_date), unit = "month")),
      end = lubridate::as_date(
        lubridate::ceiling_date(max(.data$start_date), unit = "month")-1)) %>%
    tidyr::nest(date = c(.data$start, .data$end)) %>%
    mutate(
      date = purrr::map(date, ~ seq.Date(.x$start, .x$end, by = "day"))) %>%
    unnest(.data$date)

  out <- left_join(base_calendar, base_events, by = c("site", "date")) %>%
    mutate(event_count = if_else(is.na(.data$event_count), 0L, .data$event_count)) %>%
    arrange(.data$site, .data$date)
  
  out %>%
    ggplot(aes(x = date, y = event_count, colour = site, group = site)) + geom_path()
  
  outa <- out %>%
    filter(site == "A")
  
  out <- bind_rows(
    outa,
    tibble(event_count = rep(0L, 31)) %>%
      mutate(date = seq.Date(from = as.Date("2018-01-01"), by = "day", length.out = 31)),
    outa[(nrow(outa)-60):nrow(outa),] %>%
      mutate(date = seq.Date(from = as.Date("2018-02-01"), by = "day", length.out = nrow(.)))
  )
  
  out %>% 
    time_decompose(event_count) %>%
    anomalize(remainder) %>%
    time_recompose() %>%
    filter(anomaly == "Yes") 
  
  out %>%
    time_decompose(event_count, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
    plot_anomaly_decomposition()
  
  %>%
    mutate(year = lubridate::year(.data$date),
           month = lubridate::month(.data$date)) %>%
    group_by(.data$site, .data$year, .data$month) %>%
    tally() %>%
    filter(.data$n > 10) %>%
    arrange(.data$site, .data$year, .data$month)
  
  return(out)
}


hr_cov <- coverage(hrv, ref)
