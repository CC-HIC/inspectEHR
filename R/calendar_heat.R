#' Make a heat_cal object
#'
#' @param reference_tbl reference table produced from \code{\link{make_reference}}
#' @param dataitem_tbl an extracted dataitem from \code{\link{extract}}
#' @param site character string of hospital site to plot
#' @param date_boundaries character vector of format YYYY-MM-DD (or something
#'   that is parsable by \code{lubridate::ymd}) defining the start and end
#'   dates of the calendar. If nothing is supplied, this is derrived from the
#'   data
#' @param max_limit the upper bound for determining colour scale. This is useful
#'   if you have extreme outliers to enable good constrast over the range of
#'   interest
#' @export
#'
#' @return an object of class heat_cal. Returns `FALSE` if the site is not found
#'   in the data
make_heatcal <- function(
  reference_tbl = NULL,
  dataitem_tbl = NULL,
  site = NULL,
  date_boundaries = NULL,
  max_limit = NULL) {

  if (class(reference_tbl)[1] != "tbl_ref") {
    abort("You must supply a reference table. See `make_reference()`")
  }

  if (!is.null(dataitem_tbl)) {
    type <- "events"
    if (!(any(class(dataitem_tbl) %in% preserved_classes))) {
      abort("You must supply an extracted table for this functionality. See `extract()`")
    }
    if (!(site %in% unique(dataitem_tbl$site))) {
      rlang::inform("No data for this site")
      return(FALSE)
    }
  } else {
    type <- "episodes"
  }

  if (type == "episodes") {
    heatcal <- reference_tbl %>%
      daily_admissions(by_site = site) %>%
      create_calendar_template(date_boundaries = date_boundaries)
  } else {
    heatcal <- dataitem_tbl %>%
      daily_events(reference = reference_tbl, by_site = site) %>%
      create_calendar_template(date_boundaries = date_boundaries)
  }

  calendar_grid <- heatcal %>%
    create_grid()

  attr(heatcal, "grid") <- calendar_grid
  attr(heatcal, "max") <- max_limit
  attr(heatcal, "type") <- type
  attr(heatcal, "site") <- site
  class(heatcal) <- append(class(heatcal), "heat_cal", after = 0)

  return(heatcal)

}

#' Find First Sunday
#'
#' Finds the first sunday of the month supplied to x
#'
#' @param x a vector of class date
#' @return a vector with dates for the first sunday of each year supplied to x
#'
#' @importFrom lubridate floor_date wday days
find_first_sunday <- function(x) {
  first_of_month <- floor_date(x, "month")
  days_of_week <- vapply(
    seq(0, 6), function(x) {
      as.integer(
        wday(
          first_of_month + days(x)
        )
      )},
    vector(mode = "double", length = 1))
  first_sunday <- first_of_month + days(which(days_of_week == 1) - 1)
}


#' Calendar Template for Count Data
#'
#' Creates the basic template for a heatmap calendar in ggplot when using count
#' data. The calendar will automatically expand the date contrains to full
#' years. Can be supplied the output from \code{\link{daily_admissions}}.
#'
#' @param x count data to be used in the calendar from
#'   \code{\link{daily_admissions}}
#'   \describe{
#'     \item{date}{date column formatted to a POSIX date}
#'     \item{count_column}{counting column of integer type}
#'   }
#' @param date_boundaries character vector of format YYYY-MM-DD (or something
#'   that is parsable by \code{lubridate::ymd}) defining the start and end
#'   dates of the calendar. If nothing is supplied, this is derrived from the
#'   data
#'
#' @return a tibble with correct week alignments for a calendar heatmap
#' @export
#'
#' @importFrom lubridate floor_date ceiling_date ymd year month
#' @importFrom dplyr tibble mutate left_join
create_calendar_template <- function(x = NULL,
                                     date_boundaries = NULL) {

  if (is.null(date_boundaries)) {
    first_date <- floor_date(min(x$date), unit = "years")
    last_date <- ceiling_date(max(x$date), unit = "years") - 1
  } else {
    first_date <- floor_date(ymd(date_boundaries[1]), unit = "years")
    last_date <- ceiling_date(ymd(date_boundaries[2]), unit = "years") - 1
  }

  # days till first sunday for each year
  remaining <-
    tibble(
      years = seq(
        from = first_date,
        to = last_date,
        by = "year"
      )
    ) %>%
    mutate(
      first_sundays = as.Date(
        vapply(
          .data$years,
          find_first_sunday,
          vector(mode = "double", length = 1)),
        origin = "1970/01/01"
      ),
      remaining_days = as.integer(.data$first_sundays - .data$years),
      year = year(.data$years)
    )

  calendar <- tibble(
    date = seq(from = first_date, to = last_date, by = "day")
    ) %>%
    mutate(year = as.integer(year(date)))

  years <- unique(calendar$year)
  week_of_year <- vector(mode = "integer")

  for (year in years) {
    if (remaining$remaining_days[remaining$year == year] == 0) {
      this_week_of_year <- rep(1:52, each = 7)
    } else {
      this_week_of_year <- c(
        rep(1, times = remaining$remaining_days[remaining$year == year]),
        rep(2:52, each = 7)
      )
    }

    remaining_length <- nrow(
      calendar[calendar$year == year, ]) - length(this_week_of_year)

    if (remaining_length != 0) {
      this_week_of_year <- c(
        this_week_of_year, rep(53, times = remaining_length))
    }

    week_of_year <- c(week_of_year, this_week_of_year)
  }

  calendar <- calendar %>%
    mutate(
      week_of_year = as.integer(week_of_year),
      day_of_week = wday(date, label = TRUE),
      month = month(date, label = TRUE)
    ) %>%
    left_join(x, by = "date")
}

#' Calendar Gridlines
#'
#' This calulates the correct gridlines that need to be drawn when formatting
#' the calendar.
#'
#' @param calendar_template a calendar template from
#'   \code{\link{create_calendar_template}}
#'
#' @return a grid for assembling a ggheatcal
#' @importFrom dplyr select mutate lead lag distinct
create_grid <- function(calendar_template = NULL) {

  template <- calendar_template %>%
    select(.data$date, .data$year, .data$week_of_year,
           .data$day_of_week, .data$month)

  # Right Vertical
  rv <- template %>%
    mutate(
      x_start = if_else(
        lead(.data$month, 7) != .data$month |
          is.na(lead(.data$month, 7)),
        .data$week_of_year + 0.5,
        as.double(NA)
      ),
      y_start = if_else(
        lead(.data$month, 7) != .data$month |
          is.na(lead(.data$month, 7)),
        as.double(.data$day_of_week) + 0.5,
        as.double(NA)
      )
    ) %>%
    na.omit() %>%
    select(.data$year, .data$x_start, .data$y_start) %>%
    mutate(
      x_end = .data$x_start,
      y_end = .data$y_start - 1
    )

  # Left Vertical
  lv <- template %>%
    mutate(
      x_start = if_else(lag(.data$month, 7) != .data$month |
        is.na(lag(.data$month, 7)), .data$week_of_year - 0.5, as.double(NA)),
      y_start = if_else(
        lag(.data$month, 7) != .data$month |
          is.na(lag(.data$month, 7)),
        as.double(day_of_week) + 0.5,
        as.double(NA)
      )
    ) %>%
    na.omit() %>%
    select(.data$year, .data$x_start, .data$y_start) %>%
    mutate(
      x_end = .data$x_start,
      y_end = .data$y_start - 1
    )

  # Top Horizontal
  th <- template %>%
    mutate(
      x_start = if_else(
        lead(.data$month, 1) != .data$month |
          lead(.data$week_of_year, 1) != .data$week_of_year |
          is.na(lead(.data$week_of_year)),
        week_of_year - 0.5,
        as.double(NA)
      ),
      y_start = if_else(
        lead(.data$month, 1) != .data$month |
          lead(.data$week_of_year, 1) != .data$week_of_year |
          is.na(lead(.data$week_of_year)),
        as.double(.data$day_of_week) + 0.5,
        as.double(NA)
      )
    ) %>%
    na.omit() %>%
    select(.data$year, .data$x_start, .data$y_start) %>%
    mutate(
      x_end = .data$x_start + 1,
      y_end = .data$y_start
    )

  # Bottom Horizontal
  bh <- template %>%
    mutate(
      x_start = if_else(
        lag(.data$month, 1) != .data$month |
          lag(.data$week_of_year, 1) != .data$week_of_year |
          is.na(lag(.data$week_of_year, 1)),
        .data$week_of_year - 0.5,
        as.double(NA)
      ),
      y_start = if_else(
        lag(.data$month, 1) != .data$month |
          lag(.data$week_of_year, 1) != .data$week_of_year |
          is.na(lag(.data$week_of_year, 1)),
        as.double(.data$day_of_week) - 0.5,
        as.double(NA)
      )
    ) %>%
    na.omit() %>%
    select(.data$year, .data$x_start, .data$y_start) %>%
    mutate(
      x_end = .data$x_start + 1,
      y_end = .data$y_start
    )

  grid_lines <- rbind(rv, lv, th, bh) %>% distinct()
}
