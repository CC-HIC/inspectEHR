#' Summary for flagged data item
#'
#' @param x
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise full_join
#' @importFrom tidyr gather
#'
#' @examples
#' summary(x)
summary_main <- function(x, reference) {

  dl <- vector(mode = "list", length = 2)
  names(dl) <- c("error_checks", "missingness")

  bounds <- x %>%
    group_by(.data$site) %>%
    summarise(
      early_event = sum(
        ifelse(.data$out_of_bounds == 101, 1L, 0L), na.rm = TRUE),
      late_event = sum(
        ifelse(.data$out_of_bounds == 102, 1L, 0L), na.rm = TRUE))

  range <- x %>%
    group_by(.data$site) %>%
    summarise(
      low_value = sum(
        ifelse(.data$range_error == 103, 1L, 0L), na.rm = TRUE),
      high_value = sum(
        ifelse(.data$range_error == 104 | .data$range_error == 105, 1L, 0L), na.rm = TRUE))

  dup <- x %>%
    group_by(.data$site) %>%
    summarise(
      duplicate_events = sum(
        ifelse(.data$duplicate == 106, 1L, 0L), na.rm = TRUE))

  dl[["error_checks"]] <- bounds %>%
    full_join(range, by = "site") %>%
    full_join(dup, by = "site") %>%
    gather("error_type", "n", -.data$site)

  if (any(grepl("1d", class(x)))) {

    dl[["missingness"]] <- x %>%
      missingness(reference)

  } else {

    dl[["missingness"]] <- NA

  }

  return(dl)

}


#' Check for 1d Missingness
#'
#' @param x
#'
#' @return a tibble with the following columns:
#' \describe{
#'   \item{event}{name of the CC-HIC event}
#'   \item{site}{name of the BRC from which the event originates}
#'   \item{count}{total count of validated events recieved}
#'   \item{total}{total popululation}
#'   \item{missingness}{1 - count/total}
#' }
#' @export
#'
#' @importFrom rlang !! .data
#' @importFrom dplyr filter group_by summarise
#'
#' @examples
#' missingness(df)
missingness <- function(x, reference) {

  x <- x %>%
    dplyr::filter(.data$range_error == 0 | is.na(.data$range_error),
                  .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
                  .data$duplicate == 0 | is.na(.data$duplicate)) %>%
    dplyr::group_by(.data$code_name, .data$site) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::left_join(reference %>%
                       group_by(site) %>%
                       summarise(total = n()), by = "site") %>%
    mutate(missingness = 1 - count/total)

  return(x)

}

