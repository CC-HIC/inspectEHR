#' Clean Table
#'
#' This function takes an extracted table from either
#' \code{\link{extract_demographics}} or \code{\link{extract_timevarying}} and
#' returns the table with values that are out of range or impossible (as defined
#' in the data quality reference table)
#'
#' @param tbl a table extracted from the CC-HIC database
#' @param col_names a vector of the names you want to change
#' @param dq_ref a tibble if you want to override the default boundary conditions
#'   with the following column headings:
#'   code_names
#'   min_range
#'   max_range
#'
#' @return
#' @export
#'
#' @examples
clean <- function(tbl = NULL, col_names) {

  imap

  for (i in seq_along(col_names)) {
    tbl <- cleaning_helper(tbl, col_name = col_names[i])
  }

}

cleaning_helper <- function(df, col_name, action = "NA") {

  rng <- qref %>%
    filter(.data$code_name == col_name) %>%
    select(.data$range_min, .data$range_max) %>%
    unlist()

  type_change <- qref %>%
    filter(.data$code_name == col_name) %>%
    select(.data$primary_column) %>%
    pull()

  if (type_change == "integer") {
    type_change <- as.integer(NA)
  } else if (type_change == "real") {
    type_change <- as.numeric(NA)
  } else {
    rlang::abort("Methods are not yet defined for this data type")
  }

  if (action == "NA") {
    df <- df %>%
      mutate(new = if_else(
        .data[[col_name]] < rng["range_min"] | .data[[col_name]] > rng["range_max"],
        type_change, .data[[col_name]]))
  } else if (action == "limits") {
    df <- df %>%
      mutate(new = case_when(
        .data[[col_name]] < rng["range_min"] ~ rng["range_min"],
        .data[[col_name]] > rng["range_max"] ~ rng["range_max"],
        TRUE ~ .data[[col_name]]
      )
      )
  }

  return(df)

}
