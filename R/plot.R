#' Plot CC-HIC Data item eCDF
#'
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom stringr str_trunc
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
plot_ecdf <- function(x, sites_col = NULL, verify = TRUE) {

  value_title <- gsub("_", " ", attr(x, "code_name"))
  subtitle <- str_trunc(
    qref[qref$code_name == attr(x, "code_name"), "short_name", drop = TRUE],
    40)
  x_lab <- qref[qref$code_name == attr(x, "code_name"), "assumed_units",
                drop = TRUE]
  if (is.na(x_lab)) x_lab <- "Units not defined"

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
  }

  if (verify) {
    x <- x %>%
      filter(
        .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
        .data$duplicate == 0 | is.na(.data$duplicate),
        .data$range_error == 0 | is.na(.data$range_error)
      )
  }
  x <- x %>%
    ggplot(aes(x = .data$value, colour = .data$site)) +
    scale_colour_manual(values = sites_col) +
    stat_ecdf() +
    xlab(x_lab) +
    ylab("F(x)") +
    ggtitle(value_title, subtitle = subtitle) +
    theme_cchic() +
    guides(colour = guide_legend(title = "Site"))

  attr(x, "plot_type") <- "ecdf"
  invisible(x)
}


#' Plot Histogram
#'
#' Plot a histogram of a CC-HIC event, with each site normalised to 100%
#'
#' @param x an extracted hic data item
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tidyr complete
#' @importFrom dplyr filter select group_by ungroup mutate summarise
#' @importFrom scales percent_format
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous xlab ylab theme_minimal aes
plot_hist <- function(x, sites_col = NULL, verify = TRUE) {

  code_name <- attr(x, "code_name")

  value_title <- gsub("_", " ", attr(x, "code_name"))
  x_lab <- qref[qref$code_name == attr(x, "code_name"), "assumed_units"]
  if (is.na(x_lab)) x_lab <- "Units not defined"

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
  }

  if (verify) {
    x <- x %>%
      filter(
        .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
        .data$duplicate == 0 | is.na(.data$duplicate),
        .data$range_error == 0 | is.na(.data$range_error)
      )
  }

  x <- x %>%
    select(.data$site, .data$value) %>%
    group_by(.data$site, .data$value) %>%
    summarise(n = n()) %>%
    group_by(.data$site) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    complete(site, value) %>%
    ggplot(aes(x = factor(.data$value), fill = .data$site)) +
    geom_bar(aes(y = .data$n / .data$total),
             position = "dodge",
             stat = "identity",
             width = 0.8) +
    scale_fill_manual(values = sites_col) +
    scale_y_continuous(labels = percent_format()) +
    xlab(x_lab) +
    ylab("Percentage by Site") +
    ggtitle(value_title) +
    theme_cchic() +
    guides(fill = guide_legend(title = "Site"))

  attr(x, "plot_type") <- "hist"
  invisible(x)
}


#' Plot periodicity
#'
#' Plot the periodicity for a CC-HIC event. This is the typical
#' number of entries for the event per patient per 24 hours. Please note,
#' this can ONLY plot verified events, as the periodicity calculation is
#' conditional on there being a well defined episode start and end.
#'
#' @param x and extracted and verified dataitem
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter distinct
#' @importFrom ggplot2 ggplot aes scale_fill_manual xlab ylab
#' theme_minimal geom_histogram
plot_periodicity <- function(x, sites_col = NULL, verify = TRUE) {


  value_title <- gsub("_", " ", attr(x, "code_name"))
  subtitle <- "Dataitem periodicity"
  x_lab <- "Typical events contributed per 24 hours"
  if (is.na(x_lab)) x_lab <- "Units not defined"

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
  }

  if (verify) {
    x <- x %>%
      filter(
        .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
        .data$duplicate == 0 | is.na(.data$duplicate),
        .data$range_error == 0 | is.na(.data$range_error)
      )
  }
  x <- x %>%
    distinct(.data$site, .data$periodicity) %>%
    filter(.data$periodicity <= 72) %>%
    ggplot(aes(x = .data$periodicity, colour = .data$site)) +
    scale_colour_manual(values = sites_col) +
    stat_ecdf() +
    xlab(x_lab) +
    ylab("F(x)") +
    ggtitle(value_title, subtitle = subtitle) +
    theme_cchic() +
    guides(colour = guide_legend(title = "Site"))

  attr(x, "plot_type") <- "period"
  invisible(x)
}


#' Plot Kolmogorov-Smirnov Test
#'
#'
#' Produces a plot of the KS distances between sites.
#' This is quite hard coded at the moment, and so could be improved in a future version
#'
#' @param x an object from \code{ks_test}
#' @param reference_tbl reference table from \code{\link{make_reference}}
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate select bind_rows mutate_at vars funs
#' @importFrom ggplot2 aes geom_tile scale_fill_viridis_c theme_minimal coord_equal ylab xlab geom_text
#' @importFrom scales viridis_pal
plot_ks <- function(x, reference_tbl) {

  value_title <- gsub("_", " ", attr(x, "code_name"))
  subtitle <- "Dataitem Kolmogorov-Smirnov comparisons"

  sites <- na.omit(unique(reference_tbl$site))
  df <- select(x, .data$Site_A, .data$Site_B, .data$statistic)

  ## Add in a manual KS distance from sites that are the same.
  ## Adds a visual clue of ground zero.
  df <- bind_rows(
    df,
    tibble(
      Site_A = sites,
      Site_B = sites,
      statistic = 0
    )
  )

  out <- df %>%
    ggplot(aes(x = .data$Site_A, y = .data$Site_B)) +
    geom_tile(aes(fill = .data$statistic)) +
    scale_fill_viridis_c(limits = c(0, 1)) +
    geom_text(aes(label = round(.data$statistic, 2)), colour = "white") +
    coord_equal() +
    ylab("Comparitor Site A") +
    xlab("Comparitor Site B") +
    guides(fill = guide_legend(title = "KS Distance")) +
    theme_cchic() +
    ggtitle(value_title, subtitle = subtitle)

  return(out)
}

#' Plot Integer
#'
#' Plot String values from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.integer_1d <- function(x, sites_col = NULL, verify = TRUE) {
  code_name <- attr(x, "code_name")
  if (code_name %in% categorical_hic) {
    x <- plot_hist(x = x, sites_col = sites_col, verify = verify)
  } else {
    x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
  }
}

#' @importFrom ggplot2 autoplot
plot.integer_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}


#' Plot Real Valued Dataitem
#'
#' Plot real valued dataitem from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.real_1d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.real_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}

#' Plot String
#'
#' Plot String values from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.string_1d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_hist(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.string_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}


#' Plot Datetime
#'
#' Plot datetime values from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.date_1d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.date_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}


#' Plot Datetime
#'
#' Plot datetime values from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.datetime_1d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.datetime_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}

#' Plot Time
#'
#' Plot time values from CC-HIC
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.time_1d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.time_1d <- function(x, display = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
  }
  invisible(lst)
  }
}


#' Plot String
#'
#' Plot String values from CC-HIC.
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.string_2d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_hist(x = x, sites_col = sites_col, verify = verify)
}


#' @importFrom ggplot2 autoplot
plot.string_2d <- function(x, display = TRUE, periodicity = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
      print(lst[[plot_type]])
    }
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
    }
  }
  invisible(lst)
  }
}


#' Plot Integer
#'
#' Plot integer values from CC-HIC
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.integer_2d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.integer_2d <- function(x, display = TRUE, periodicity = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
      print(lst[[plot_type]])
    }
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
    }
  }
  invisible(lst)
  }
}


#' Plot Real
#'
#' Plot real values from CC-HIC
#'
#' @param x a varified data frame
#' @param sites_col the HEX colour pallet for each site
#' @param verify \code{logical} if use only verified data
#'
#' @importFrom rlang .data quo !!
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab theme_minimal ggtitle guides guide_legend
autoplot.real_2d <- function(x, sites_col = NULL, verify = TRUE) {
  x <- plot_ecdf(x = x, sites_col = sites_col, verify = verify)
}

#' @importFrom ggplot2 autoplot
plot.real_2d <- function(x, display = TRUE, periodicity = TRUE, ...) {
  if (nrow(x) == 0) {
    rlang::warn("There is no data in this table...")
  } else {
  lst <- list()
  if (display) {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    print(lst[[plot_type]])
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
      print(lst[[plot_type]])
    }
  } else {
    y <- autoplot(x, ...)
    plot_type <- attr(y, "plot_type")
    lst[[plot_type]] <- y
    if (periodicity) {
      y <- plot_periodicity(x, ...)
      plot_type <- attr(y, "plot_type")
      lst[[plot_type]] <- y
    }
  }
  invisible(lst)
  }
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

#' Plot Calendar Heatmap
#'
#' Builds the ggplot layers required to correctly display the calendar heatmap
#'
#' @param object a heat_cal object
#' @param ... other arguments to be passed
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn facet_grid
#' theme_minimal theme element_blank element_text geom_segment aes labs
#' ylab xlab coord_equal
#' @importFrom scales viridis_pal
#' @importFrom rlang .data
autoplot.heat_cal <- function(object, ...) {

  type <- attr(object, "type")
  max_limit <- attr(object, "max")
  title <- paste0("Calendar Heatmap: ", attr(object, "site"))
  grid_lines <- attr(object, "grid")

  if (is.null(max_limit)) max_limit <- max(object[type], na.rm = TRUE)

  if (type == "episodes") {
    guide_title <- "# admit"
    subtitle <- "Admission profile"
  } else {
    guide_title <- "# event"
    subtitle <- "Data submission profile"
  }

  object %>%
    ggplot() +
    geom_tile(aes(
      x = .data$week_of_year,
      y = .data$day_of_week,
      fill = .data[[type]]),
      colour = "#FFFFFF") +
    facet_grid(.data$year ~ .) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    geom_segment(
      aes(x = .data$x_start, y = .data$y_start,
          xend = .data$x_end, yend = .data$y_end),
      colour = "black", size = 0.5,
      lineend = "round", linejoin = "round",
      data = grid_lines
    ) +
    scale_fill_viridis_c(
      rescaler = function(x, to = c(0, 1), from = NULL) {
        if_else(
          x <= max_limit,
          scales::rescale(
            x, to = to, from = c(min(x, na.rm = TRUE), max_limit)), 1
      )}, na.value = "grey60") +
    labs(title = title, subtitle = subtitle) +
    ylab(label = "Day of Week") +
    xlab(label = "Month") +
    guides(fill = guide_legend(
      title = guide_title)) +
    coord_equal()

}

#' @importFrom ggplot2 autoplot
plot.heat_cal <- function(x, display = TRUE, ...) {
  if (display) {
    print(autoplot(x, ...))
  } else {
    autoplot(x, ...)
  }
}


#' CC-HIC ggplot2 Theme
#'
#' @param ... arguments to pass to \code{theme}
#'
#' @export
#'
#' @importFrom ggplot2 %+replace% theme theme_bw element_blank
theme_cchic <- function(...) {
  pct <- theme_bw(base_family = "sans", base_size = 11) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank()
    )
}
