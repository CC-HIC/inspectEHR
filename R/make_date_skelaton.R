make_date_skelaton <- function() {

  date_range <- seq.Date(dmy("01-01-2014"), Sys.Date(), by = 1)

  date_skelaton <- tibble(
    site = c(rep("Oxford", length(date_range)),
             rep("RYJ", length(date_range)),
             rep("GSTT", length(date_range)),
             rep("UCL", length(date_range)),
             rep("RGT", length(date_range))),
    date = rep(date_range, 5)) %>%
    mutate(
      wday = wday(date, label = TRUE),
      week_of_month = as.integer(ceiling(day(date)/7)),
      month = month(date, label = TRUE),
      year = year(date))

}

