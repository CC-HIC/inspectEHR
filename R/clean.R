#' Clean Table
#'
#' This function takes an extracted table from either
#' \code{\link{extract_demographics}} or \code{\link{extract_timevarying}} and
#' returns the table having dealt with out of range values. Currently this
#' will either modify the value to NA, or limit the value at an appropraite max
#' or min.
#'
#' @param tbl a table extracted from the CC-HIC database
#' @param dq_ref a tibble if you want to override the default boundary
#'   conditions with the following column headings:
#'   \describe{
#'     \item{code_name}{the code name for the data item as it appears in your
#'       table}
#'     \item{range_min}{the lower bound you want to set}
#'     \item{range_max}{the upper bound you want to set}
#'   }
#' @return a table of the same dimentions as \code{tbl} but with values cleaned
#' @export
clean <- function(tbl = NULL, dq_ref = NULL) {
  tbl <- purrr::imap_dfc(tbl, .f = cleaning_helper, dq_ref = dq_ref)
}

cleaning_helper <- function(col_vec, col_name, dq_ref = NULL, action = "NA") {

  if (is.null(dq_ref)) {
    dq <- qref
  } else {
    dq <- dq_ref
  }

  dataitem_check <- nrow(dq[dq$code_name == col_name,])
  if (dataitem_check != 1) {
    rlang::inform(
      paste0("Cannot clean for column name: ", col_name)
    )
    return(col_vec)
  } else {

  quo_name <- enquo(col_name)

  rng <- dq %>%
    filter(.data$code_name == col_name) %>%
    select(.data$range_min, .data$range_max) %>%
    unlist()

  type_change <- class(col_vec)

  if (type_change == "integer") {
    type_change <- as.integer(NA)
  } else if (type_change == "numeric") {
    type_change <- as.numeric(NA)
  } else if (type_change == "character") {
    type_change <- as.character(NA)
  } else {
    rlang::abort("Methods are not yet defined for this data type")
  }

  if (action == "NA") {
    before <- sum(is.na(col_vec))
    if (class(type_change) == "character") {
      pos_vales <- qref %>%
        filter(.data$code_name == col_name) %>%
        select(.data$possible_values) %>%
        unnest() %>%
        pull()
      col_vec <- if_else(col_vec %in% pos_vales, col_vec, type_change)
    } else {
    col_vec <- if_else(
      col_vec < rng["range_min"] | col_vec > rng["range_max"],
      type_change, col_vec)
    }
    after <- sum(is.na(col_vec))
    change <- after - before
  } else if (action == "limits") {
    before <- col_vec
    if (class(type_change) == "character") {
      rlang::inform(
        paste0(
          "limit action is not defined for: ", col_name, ". using NA instead")
      )
      pos_vales <- qref %>%
        filter(.data$code_name == col_name) %>%
        select(.data$possible_values) %>%
        unnest() %>%
        pull()
      col_vec <- if_else(col_vec %in% pos_vales, col_vec, type_change)
    } else {
      col_vec <- pmin(col_vec, rng["range_max"])
      col_vec <- pmax(col_vec, rng["range_min"])
    }
    change <- length(setdiff(before, col_vec))
  }

  return(col_vec)
  rlang::inform(
    paste0("Finished cleaning: ", col_name, ". ", change, "elements modified")
  )
  }

}

#' Clean Column
#'
#' @param df table to clean
#' @param col_name column name to clean
#' @param action action of the cleaner
#'
#' @importFrom rlang :=
clean_column <- function(df, col_name, action = "NA") {

  dataitem_check <- nrow(qref[qref$code_name == col_name,])
  if (dataitem_check != 1) {
    rlang::inform(
      paste0("Cannot clean for column name: ", col_name)
    )
    return(df)
  } else {

    quo_name <- enquo(col_name)

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
        mutate(!!quo_name := if_else(
  .data[[col_name]] < rng["range_min"] | .data[[col_name]] > rng["range_max"],
          type_change, .data[[col_name]]))

    } else if (action == "limits") {
      df <- df %>%
        mutate(!!quo_name := case_when(
          .data[[col_name]] < rng["range_min"] ~ rng["range_min"],
          .data[[col_name]] > rng["range_max"] ~ rng["range_max"],
          TRUE ~ .data[[col_name]]
        )
        )
    }
    return(df)
    rlang::inform(
      paste0("Finished cleaning: ", col_name)
    )
  }

}
