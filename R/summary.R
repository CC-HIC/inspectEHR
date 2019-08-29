#' Collate and Summarise Verification Information
#'
#' @param x a verified dataitem from \code{\link{verify_events}}
#' @param reference the reference table from \code{\link{make_reference}}
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarise full_join n if_else
#' @importFrom tidyr gather
summarise_verification <- function(verified, stats, coverage, reference) {
  dl <- vector(mode = "list", length = 4)
  names(dl) <- c("error_checks", "stats", "completeness", "coverage")

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

  dl[["error_checks"]] <- bounds %>%
    full_join(range, by = "site") %>%
    full_join(dup, by = "site") %>%
    gather("error_type", "n", -.data$site)

  dl[["stats"]] <- stats

  if (any(grepl("1d", class(verified)))) {
    dl[["completeness"]] <- verify_complete(verified, reference)
    dl[["coverage"]] <- coverage(verified, reference)
  } else {
    dl[["completeness"]] <- NA
    dl[["coverage"]] <- coverage(verified, reference)
  }

  return(dl)
}
