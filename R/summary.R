#' Collate and Summarise Verification Information
#'
#' @param verified a verified dataitem from \code{\link{verify_events}}
#' @param stats the stats verification or NULL
#' @param coverage the coverage verification or NULL
#' @param reference the reference table from \code{\link{make_reference}}
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise full_join n if_else
#' @importFrom tidyr gather
summarise_verification <- function(verified, stats, coverage, reference) {
  dl <- vector(mode = "list", length = 5)
  names(dl) <- c("n", "error_checks", "stats", "completeness", "coverage")

  n <- verified %>%
    group_by(.data$site) %>%
    tally()

  bounds <- verified %>%
    group_by(.data$site) %>%
    summarise(
      early_event = sum(
        if_else(.data$out_of_bounds == -1, 1L, 0L),
        na.rm = TRUE
      ),
      late_event = sum(
        if_else(.data$out_of_bounds == 1, 1L, 0L),
        na.rm = TRUE
      )
    )

  range <- verified %>%
    group_by(.data$site) %>%
    summarise(
      low_value = sum(
        if_else(.data$range_error == -1, 1L, 0L),
        na.rm = TRUE
      ),
      high_value = sum(
        if_else(.data$range_error == 1, 1L, 0L),
        na.rm = TRUE
      )
    )

  dup <- verified %>%
    group_by(.data$site) %>%
    summarise(
      duplicate_events = sum(
        if_else(.data$duplicate == 1, 1L, 0L),
        na.rm = TRUE
      )
    )

  dl[["n"]] <- n

  dl[["error_checks"]] <- bounds %>%
    full_join(range, by = "site") %>%
    full_join(dup, by = "site") %>%
    gather("error_type", "n", -.data$site)

  dl[["stats"]] <- stats
  dl[["coverage"]] <- coverage

  if (any(grepl("1d", class(verified)))) {
    dl[["completeness"]] <- verify_complete(verified, reference)
  } else {
    dl[["completeness"]] <- NA
  }

  return(dl)
}
