#' Plot Calendar Heatmap
#'
#' @param episodes collected episodes table
#' @param provenance collected provenance table
#' @param site character string of hospital site to plot
#' @param filename optional string to save output. must include extension
#'
#' @return a plot with admission details in heatmap form
#' @export
#'
#' @examples
#' plot_heatcal(episodes, provenance, "UCL")
#' plot_heatcal(episodes, provenance, "UCL", "~/some/path/plot.png")
plot_heatcal <- function(reference_table = NULL,
                         site = NULL,
                         filename = NULL,
                         start_date = "2014-01-01",
                         end_date = "2019-01-01",
                         max_limit = 25) {

  calendar_template <- reference_table %>%
    daily_admssions(by_site = site) %>%
    create_calendar_template(start_date = start_date, end_date = end_date)

  calendar_grid <- calendar_template %>%
    create_grid()

  if (is.null(filename)) {

    plotted_heatcal <- ggHeatCal_episodes(x = calendar_template,
              gridLines = calendar_grid,
              Title = paste0("Admission Calendar Heatmap for " , site),
              max_limit = max_limit)

    return(plotted_heatcal)

  } else {

    ggsave(
      ggHeatCal_episodes(x = calendar_template,
                gridLines = calendar_grid,
                Title = paste0("Admission Calendar Heatmap for " , site),
                max_limit = max_limit),
      filename = filename)

  }

}


#' Find First Sunday
#'
#' Finds the first sunday of the year supplied to x
#'
#' @param x a vector of class date
#'
#' @return a vector with dates for the first sunday of each year supplied to x
#'
#' @importFrom lubridate floor_date wday days
#'
#' @examples
#' find_first_sunday(x)
find_first_sunday <- function(x) {

  first <- floor_date(x, "month")
  dow <- sapply(seq(0,6),function(x) wday(first+days(x)))
  firstSunday <- first + days(which(dow==1)-1)
  return(firstSunday)

}


#' Create a Calendar Template for Count Data
#'
#' Creates the basic template for a heatmap calendar in ggplot
#' when using count data. The calendar will automatically expand
#' the date contrains to full years
#'
#' @param start_date a starting date as character vector of format YYYY/MM/DD
#' @param end_date an end date as character vector of format YYYY/MM/DD
#' @param x count data to be used in the calendar
#'   \describe{
#'     \item{date}{date column formatted to a POSIX date}
#'     \item{count_column}{counting column of integer type}
#'   }
#'
#' @return a tibble with correct week alignments for a calendar heatmap
#' @export
#'
#' @importFrom lubridate floor_date ceiling_date ymd year month
#' @importFrom dplyr tibble mutate left_join
#'
#' @examples
#' sample_data <- create_calendar_template(myData, "2014-01-01", "2018-01-01")
create_calendar_template <- function(x = NULL,
                                     start_date = "2014-01-01",
                                     end_date = "2018-01-01") {

  first_date <- floor_date(ymd(start_date), unit = "years")
  last_date <- ceiling_date(ymd(end_date), unit = "years") - 1

  # days till first sunday for each year
  remaining <-
    tibble(
      years = seq(from = first_date,
                    to = last_date,
                    by = "year")) %>%
    mutate(
      firstSundays = as.Date(
        sapply(years, find_first_sunday), origin = "1970/01/01"),
    remaining_days = as.integer(firstSundays - years),
              year = year(years))

  calendar <-
    tibble(date = seq(from = first_date, to = last_date, by = "day")) %>%
    mutate(year = as.integer(year(date)))

  years <- unique(calendar$year)
  week_of_year <- vector(mode = "integer")

  for (year in years) {

    if (remaining$remaining_days[remaining$year == year] == 0) {
      this_week_of_year <- rep(1:52, each = 7)

    } else {

      this_week_of_year <- c(rep(1, times = remaining$remaining_days[remaining$year == year]),
                             rep(2:52, each = 7))

    }

    remaining_length <- nrow(calendar[calendar$year == year,]) - length(this_week_of_year)

    if (remaining_length != 0) {

      this_week_of_year <- c(this_week_of_year, rep(53, times = remaining_length))

    }

    week_of_year <- c(week_of_year, this_week_of_year)

  }

  calendar <- calendar %>%
    mutate(week_of_year = as.integer(week_of_year),
            day_of_week = wday(date, label = TRUE),
                  month = month(date, label = TRUE)) %>%
    left_join(x, by = "date")

  return(calendar)

}

#' Create Calendar Grid
#'
#' This calulates the correct gridlines that need to be drawn when formatting
#' the calendar.
#'
#' @param calendar_template a calendar template from \code{create_calendar_template}
#'
#' @return a grid for assembling a ggheatcal
#' @importFrom dplyr select mutate lead lag distinct
#'
#' @examples
#' create_grid(cal_template)
create_grid <- function(calendar_template = NULL) {

  template <- calendar_template %>%
    select(date, year, week_of_year, day_of_week, month)

  # Right Vertical
  rv <- template %>%
    mutate(
      x_start = ifelse(
        lead(month, 7) != month |
          is.na(lead(month, 7)),
        week_of_year + 0.5,
        NA),
      y_start = ifelse(
        lead(month, 7) != month |
          is.na(lead(month, 7)),
        as.double(day_of_week) + 0.5,
        NA
      )
    ) %>%
    na.omit %>%
    select(year, x_start, y_start) %>%
    mutate(x_end = x_start,
           y_end = y_start - 1)

  # Left Vertical
  lv <- template %>%
    mutate(
      x_start = ifelse(lag(month, 7) != month |
                         is.na(lag(month, 7)), week_of_year - 0.5, NA),
      y_start = ifelse(
        lag(month, 7) != month |
          is.na(lag(month, 7)),
        as.double(day_of_week) + 0.5,
        NA
      )
    ) %>%
    na.omit %>%
    select(year, x_start, y_start) %>%
    mutate(x_end = x_start,
           y_end = y_start - 1)

  # Top Horizontal
  th <- template %>%
    mutate(
      x_start = ifelse(
        lead(month, 1) != month |
          lead(week_of_year, 1) != week_of_year |
          is.na(lead(week_of_year)),
        week_of_year - 0.5,
        NA
      ),
      y_start = ifelse(
        lead(month, 1) != month |
          lead(week_of_year, 1) != week_of_year |
          is.na(lead(week_of_year)),
        as.double(day_of_week) + 0.5,
        NA
      )
    ) %>%
    na.omit %>%
    select(year, x_start, y_start) %>%
    mutate(x_end = x_start + 1,
           y_end = y_start)

  # Bottom Horizontal
  bh <- template %>%
    mutate(
      x_start = ifelse(
        lag(month, 1) != month |
          lag(week_of_year, 1) != week_of_year |
          is.na(lag(week_of_year, 1)),
        week_of_year - 0.5,
        NA
      ),
      y_start = ifelse(
        lag(month, 1) != month |
          lag(week_of_year, 1) != week_of_year |
          is.na(lag(week_of_year, 1)),
        as.double(day_of_week) - 0.5,
        NA
      )
    ) %>%
    na.omit %>%
    select(year, x_start, y_start) %>%
    mutate(x_end = x_start + 1,
           y_end = y_start)

  gridLines <- rbind(rv, lv, th, bh) %>% distinct()

  return(gridLines)

}


#' Plot Calendar Heatmap - Admission episodes
#'
#' Builds the ggplot layers required to correctly display the calendar heatmap
#'
#' @param x admissions object
#' @param gridLines gridlines object
#' @param Title title of the final plot
#' @param max_limit the maximim limit of the scale to display
#'
#' @return a ggplot2 object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn facet_grid
#' theme_minimal theme element_blank element_text geom_segment aes labs
#' ylab xlab coord_equal
#' @importFrom scales viridis_pal
#'
#' @examples
#' ggHeatCal_episodes(x, gridlines, "UCL Admission Heatmap")
ggHeatCal_episodes <- function(x, gridLines, Title, max_limit = 25) {

  x %>%
    ggplot() +
    geom_tile(aes(x = week_of_year, y = day_of_week, fill = episodes), colour = "#FFFFFF") +
    facet_grid(year~.) +
    theme_minimal() +
    theme(panel.grid.major=element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 colour = "black", size = 0.5, data = gridLines) +
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      if_else(x < max_limit,
              scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), max_limit)), 1)
    }, na.value = "grey60") +
    labs(title = Title) +
    ylab(label = "Day of Week") +
    xlab(label = "Month") +
    coord_equal()

}


#' Plot calendar heatmap for events
#'
#' plots a cc-hic event over a calendar heatmap for inspection
#'
#' @param x modified hic event from \code{\link{event_occurances}}
#' @param gridLines gridlines to be made by \code{\link{create_grid}}
#' @param Title title of the plot
#' @param max_limit the maximim limit of the scale to display
#'
#' @return a ggplot2 object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn facet_grid
#' theme_minimal theme element_blank element_text geom_segment aes labs
#' ylab xlab coord_equal
#' @importFrom scales viridis_pal
#'
#' @examples
#' ggHeatCal_events(df, gridlines, "Heart Rate Heatmap")
ggHeatCal_events <- function(x, gridLines, Title, max_limit = 24) {

  x %>%
    ggplot() +
    geom_tile(aes(x = week_of_year, y = day_of_week, fill = events), colour = "#FFFFFF") +
    facet_grid(year~.) +
    theme_minimal() +
    theme(panel.grid.major=element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 colour = "black", size = 0.5, data = gridLines) +
    scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
      if_else(x < max_limit,
              scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), max_limit)), 1)
    }, na.value = "grey60") +
    labs(title = Title) +
    ylab(label = "Day of Week") +
    xlab(label = "Month") +
    coord_equal()
}
