#' Plot NHIC Events
#'
#' Plots NHIC events in a predetermined way and output to the plot folder
#' specified
#'
#' @param x flagged table
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @return A tibble 1 row per event
plot_hic <- function(x, path_name = NULL, all_sites.col,
                     start_date = "2014-01-01", end_date = "2019-01-01") {
  if (is.null(path_name)) stop("please supply a path name")

  this_event <- attr(x, "code_name")

  data_class <- qref %>%
    dplyr::filter(.data$code_name == this_event) %>%
    dplyr::select(.data$class) %>%
    dplyr::pull()

  data_type <- qref %>%
    dplyr::filter(.data$code_name == this_event) %>%
    dplyr::select(.data$primary_column) %>%
    dplyr::pull()

  if (nrow(x) != 0) {

    ## We should not be outputting these values as they
    ## are confidential - post codes etc.
    if (!(this_event %in% paste0(
      "NIHR_HIC_ICU_0", c(
        "001", "002", "003", "004",
        "005", "073", "076", "399",
        "088", "912"
      )
    ))) {

      # Picks out the correct main plot
      primary_plot <- plot_default(
        x,
        code_name = this_event,
        all_sites.col = all_sites.col
      )

      cowplot::ggsave(
        plot = primary_plot,
        filename = paste0(path_name, "/", this_event, "_main.png"),
        dpi = 300, width = 6, height = 4, units = "in"
      )

      # Picks out the correct full plot, warts and all.
      full_plot <- plot_full(
        x,
        code_name = this_event,
        all_sites.col = all_sites.col
      )

      cowplot::ggsave(
        plot = full_plot,
        filename = paste0(path_name, "/", this_event, "_main_full.png"),
        dpi = 300, width = 6, height = 4, units = "in"
      )

      # Check to see if there is a 2d component, and if so plot periodicity
      if (any(grepl("2d", class(x)))) {
        periodicity_plot <- plot_periodicity(x, this_event, all_sites.col)

        cowplot::ggsave(
          plot = periodicity_plot,
          filename = paste0(path_name, "/", this_event, "_periodicity.png"),
          dpi = 300, width = 6, height = 4, units = "in"
        )

        # plots out the event on a calendar heatmap

        # Capture so our axis are coordinated.
        max_daily_events <- x %>%
          mutate(date = lubridate::date(datetime)) %>%
          group_by(site, date) %>%
          tally() %>%
          ungroup() %>%
          select(n) %>%
          pull()

        max_daily_events <- max(max_daily_events, na.rm = TRUE)

        for (i in seq_along(names(all_sites.col))) {
          cal_temp <- event_occurrances(extracted_event = x, by_site = names(all_sites.col)[i]) %>%
            create_calendar_template(start_date = start_date, end_date = end_date)

          cal_grid <- create_grid(cal_temp)

          cal_plot <- ggHeatCal_events(
            cal_temp, cal_grid,
            Title = paste(this_event, "coverage for", names(all_sites.col)[i]),
            max_limit = max_daily_events
          )

          ggplot2::ggsave(
            plot = cal_plot,
            filename = paste0(path_name, "/", this_event, "_covarage_", names(all_sites.col)[i], ".png"),
            dpi = 300, width = 6, height = 4, units = "in"
          )
        }
      }
    }

    if (data_type %in% c("integer", "real")) {
      ks_t <- ks_test(x)
      if (ks_t != "less than 2 groups") {
        ks_out <- ks_plot(ks_t)
        cowplot::ggsave(
          plot = ks_out,
          filename = paste0(path_name, "/", this_event, "_ks.png"),
          dpi = 300, units = "in", width = 6, height = 4
        )
      }
    }
  } else {
    cat(
      "\n",
      this_event,
      "contains no data and will be skipped",
      "\n"
    )
  }
}


#' Default Plot
#'
#' Produce a default plot (eCDF or histogram) for a CC-HIC event code.
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @return a default plot for a single hic code
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes_string scale_fill_manual geom_density
#' xlab ylab theme_minimal
plot_default <- function(x, code_name, all_sites.col) {
  if (code_name %in% categorical_hic) {
    primary_plot <- plot_histogram_percent(x, code_name, all_sites.col)
  } else {
    primary_plot <- x %>%
      dplyr::filter(
        .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
        .data$duplicate == 0 | is.na(.data$duplicate),
        .data$range_error == 0 | is.na(.data$range_error)
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes_string(
          x = "value",
          colour = "site"
        )
      ) +
      ggplot2::stat_ecdf() +
      ggplot2::scale_colour_manual(values = all_sites.col) +
      ggplot2::xlab(code_name) +
      ggplot2::ylab("F(x)") +
      ggplot2::theme_minimal()
  }

  return(primary_plot)
}



#' Full Plot
#'
#' Plots the full distribution of a HIC event, regardless of any outliers or invalid data.
#'
#' @param x an extracted (\code{\link{extract}}) table
#' @param code_name the CC-HIC codename of interest.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes_string scale_fill_manual geom_density
#' xlab ylab theme_minimal
plot_full <- function(x, code_name, all_sites.col) {
  if (code_name %in% categorical_hic) {
    primary_plot <- plot_histogram_percent(x, code_name, all_sites.col)
  } else {
    primary_plot <- x %>%
      ggplot2::ggplot(
        ggplot2::aes_string(
          x = "value",
          colour = "site"
        )
      ) +
      ggplot2::stat_ecdf() +
      ggplot2::scale_colour_manual(values = all_sites.col) +
      ggplot2::xlab(code_name) +
      ggplot2::ylab("F(x)") +
      ggplot2::theme_minimal()
  }

  return(primary_plot)
}

#' Plot Histogram
#'
#' Plot a histogram of a CC-HIC event, with each site normalised to 100%
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tidyr complete
#' @importFrom scales percent_format
#' @importFrom ggplot2 ggplot aes_string geom_bar scale_fill_manual
#' scale_y_continuous xlab ylab theme_minimal aes
plot_histogram_percent <- function(x, code_name, all_sites.col) {
  perfect_plot <- x %>%
    dplyr::filter(
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate),
      .data$range_error == 0 | is.na(.data$range_error)
    ) %>%
    dplyr::select(.data$site, .data$value) %>%
    dplyr::group_by(.data$site, .data$value) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$site) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(site, value) %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x = "value",
        fill = "site"
      )
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = n / total),
      position = "dodge",
      stat = "identity",
      width = 0.8
    ) +
    ggplot2::scale_fill_manual(values = all_sites.col) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::xlab(code_name) +
    ggplot2::ylab("Percentage by BRC") +
    ggplot2::theme_minimal()

  return(perfect_plot)
}


#' Plot periodicity
#'
#' Plot the periodicity for a CC-HIC event. This is the typical
#' number of entries for the event per patient per 24 hours.
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter distinct
#' @importFrom ggplot2 ggplot aes_string scale_fill_manual xlab ylab
#' theme_minimal geom_histogram
plot_periodicity <- function(x, code_name, all_sites.col) {
  periodicity_plot <- x %>%
    dplyr::filter(
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate),
      .data$range_error == 0 | is.na(.data$range_error)
    ) %>%
    dplyr::distinct(.data$site, .data$periodicity) %>%
    dplyr::filter(.data$periodicity <= 48) %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x = "periodicity",
        fill = "site"
      )
    ) +
    ggplot2::scale_fill_manual(values = all_sites.col) +
    ggplot2::geom_histogram() +
    ggplot2::xlab(code_name) +
    ggplot2::ylab("Population Density") +
    ggplot2::theme_minimal()

  return(periodicity_plot)
}


#' Plot Date
#'
#' Plot dates from CC-HIC.
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
plot_date <- function(x, code_name, all_sites.col) {
  date_plot <- x %>%
    filter(
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate),
      .data$range_error == 0 | is.na(.data$range_error)
    ) %>%
    ggplot(
      aes_string(
        x = "date",
        fill = "site",
        y = "value"
      )
    ) +
    scale_fill_manual(values = all_sites.col) +
    geom_line() +
    xlab("date") +
    ylab(code_name) +
    theme_minimal()

  return(date_plot)
}


#' Plot Datetime
#'
#' Plot datetimes from CC-HIC
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
plot_datetime <- function(x, code_name, all_sites.col) {
  datetime_plot <- x %>%
    filter(
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate),
      .data$range_error == 0 | is.na(.data$range_error)
    ) %>%
    ggplot(
      aes_string(
        x = "datetime",
        fill = "site",
        y = "value"
      )
    ) +
    scale_fill_manual(values = all_sites.col) +
    geom_line() +
    xlab("datetime") +
    ylab(code_name) +
    theme_minimal()

  return(date_plot)
}


#' Plot Time
#'
#' Plot time values from CC-HIC
#'
#' @param x an extracted (using \code{\link{extract}}) and flagged (using \code{\link{flag_all}}) table
#' @param code_name the code name of interest
#' @param all_sites.col the HEX colour pallet for each site
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
plot_time <- function(x, code_name, all_sites.col) {
  time_plot <- x %>%
    filter(
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate),
      .data$range_error == 0 | is.na(.data$range_error)
    ) %>%
    ggplot(
      aes_string(
        x = "time",
        fill = "site",
        y = "value"
      )
    ) +
    scale_fill_manual(values = all_sites.col) +
    geom_line() +
    xlab("time") +
    ylab(code_name) +
    theme_minimal()

  return(date_plot)
}


# Retired occupancy plot

# plot_occupancy <- function(x) {
#
#   occ_plot <- x %>%
#     ggplot(aes(x = week_of_month, y = site, fill = est_occupancy)) +
#     geom_tile(colour = "white") +
#     scale_fill_gradient(low = "#ffffbf", high = "#d7191c", na.value = "grey70") +
#     facet_grid(year ~ month) +
#     theme_minimal(base_size = 20) +
#     theme(panel.grid = element_blank()) +
#     labs(fill = "", x = "Week of Month", y = "Biomedical Research Center")
#
#   return(occ_plot)
#
# }


# For plotting invalid months
#
# for (i in 1:length(all_sites)) {
#
#   invalid_months %>%
#     mutate(x = (month*4.3)-1,
#            y = 4,
#            text = "!") %>%
#     filter(site == all_sites[i]) -> invalids
#
#   temp_data <-
#     retrieve_unique_cases(episodes, provenance) %>%
#     report_cases_byday(bysite = all_sites[i])
#
#   temp_cal <- create_calendar(temp_data)
#   temp_grid <- create_grid(temp_cal)
#
#   temp_cal %>%
#     ggplot() +
#     geom_tile(
#       aes(x = week_of_year, y = day_of_week, fill = episodes), colour = "#FFFFFF") +
#     scale_fill_gradientn(colors = c("#B5E384", "#FFFFBD", "#FFAE63", "#D61818"), na.value = "grey90") +
#     facet_grid(year~.) +
#     geom_text(aes(x = x, y = y, label = text), colour = "red", data = invalids) +
#     theme_minimal() +
#     theme(panel.grid.major=element_blank(),
#           plot.title = element_text(hjust = 0.5),
#           axis.text.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank()) +
#     geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
#                  colour = "black", size = 0.5, data = temp_grid) +
#     labs(title = paste0("Admission Calendar Heatmap for " , names(all_sites[i]))) +
#     ylab(label = "Day of Week") +
#     xlab(label = "Month") +
#     coord_equal() -> temp_plot
#
#   ggsave(temp_plot,
#          filename = paste0("~/Projects/dataQuality/plots/admission_", all_sites[i], "_valid.png"),
#          dpi = 300)
#
#   rm(temp_data, temp_cal, temp_grid, temp_plot)
#
# }
#
# rm(i)
