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
#' @examples
#' summary(x)
summarise_verification <- function(x, reference) {
  dl <- vector(mode = "list", length = 2)
  names(dl) <- c("error_checks", "completeness")

  bounds <- x %>%
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

  range <- x %>%
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

  dup <- x %>%
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

  if (any(grepl("1d", class(x)))) {
    dl[["completeness"]] <- verify_complete(x, reference)
  } else {
    dl[["completeness"]] <- NA
  }

  return(dl)
}
