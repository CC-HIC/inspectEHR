#' Describes the coverage for reporting of a variable (S3 Generic)
#'
#' @param x an extracted nhic dataitem
#' @param ...
#'
#' @return a tibble describing the coverage of that dataitem over a defined time window
#' @export
#'
#' @examples
#' coverage(x, occupancy, all_cases)
coverage <- function(x, ...) {
  UseMethod("coverage", x)
}


#' @export
coverage.hic_dbl <- function(x, occupancy_tbl = NULL, cases_all_tbl = NULL) {

  x %<>%
    dplyr::filter(out_of_bounds == 0,
                  range_error == 0,
                  duplicate == 0) %>%
    dplyr::mutate(date = lubridate::date(datetime)) %>%
    dplyr::select(site, date, value) %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(occupancy_tbl,
                      by = c("site" = "site",
                             "date" = "date")) %>%
    dplyr::left_join(cases_all_tbl,
                     by = c("site" = "site",
                            "year" = "year",
                           "month" = "month",
                   "week_of_month" = "week_of_month")) %>%
    dplyr::select(site, date, year, month, week_of_month, wday, count, episodes, patients, est_occupancy) %>%
    dplyr::mutate(disparity = ifelse(is.na(count), NA, count/est_occupancy))

  class(x) <- c(class(x), "hic_coverage")

  return(x)

}
