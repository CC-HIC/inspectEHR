#' Extract NHIC Events
#'
#' Extracts NHIC events and appends them with the correct class for
#' further processing. This is essentially the creator method for
#' the S3 classes associated with the inspectEHR package
#'
#' @param core_table core table from make_core
#' @param input the input variable of choice
#'
#' @importFrom rlang .data !! abort
#' @importFrom magrittr %>%
#'
#' @return A tibble with 1 row per event
#' @export
#'
#' @examples
#' extract(core)
#' extract(core, input = "NIHR_HIC_ICU_0557")
extract <- function(core_table = NULL, input = NULL) {

  if (is.null(core_table)) abort("You must include the core table")
  if (!(input %in% qref$code_name)) abort("This is not a valid code")

  # Identify the correct column type to pull out
  q_type <- paste0("x", qref[qref$code_name == input, "type", drop = TRUE])
  q_class <- type <- qref[qref$code_name == input, "class", drop = TRUE]
  q_col <- qref[qref$code_name == input, "primary_column", drop = TRUE]

  # extract chosen input variable from the core table
  extracted_table <- q_type %>%
    base::switch(
      x1d = extract_1d(core_table, input = input, data_location = q_col),
      x2d = extract_2d(core_table, input = input, data_location = q_col)
    )

  class(extracted_table) <- append(class(extracted_table), q_class, after = 0)
  attr(extracted_table, "code_name") <- input

  return(extracted_table)
}


#' Extract 1d Values
#'
#' This function extracts the correct column from the CC-HIC database
#' depending upon what type of data is called for
#'
#' @param core_table the core table from make_core
#' @param input the HIC code for the variable of interest
#' @param data_location the column name that stores the primary data for this
#' variable
#'
#' @return a tibble with HIC data for a specified variable
#' @export
#'
#' @importFrom rlang .data !! sym enquo
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect select rename arrange
extract_1d <- function(core_table = NULL, input = NULL, data_location = NULL) {

  quo_column <- enquo(data_location)

  x <- core_table %>%
    filter(.data$code_name == input) %>%
    collect() %>%
    select(
      .data$event_id,
      .data$site,
      .data$code_name,
      .data$episode_id,
      !!quo_column
    ) %>%
    rename(value = !!quo_column) %>%
    arrange(.data$episode_id)

  correct_type <- qref[qref$code_name == input,"primary_column", drop = TRUE]
  ## Modify Date and Time Columns to the correct data type
  if (any(stringr::str_detect(class(core_table), "SQLite")) & (
    correct_type %in% c("datetime", "date", "time")
  )) {
    y <- correct_type %>%
      base::switch(
        datetime = modify_datetime(x),
        date = modify_date(x),
        time = modify_time(x)
      )
    return(y)
  }
  return(x)
}


#' Extract 2d Values
#'
#' This function extracts the correct column from the CC-HIC database
#' depending upon what type of data is called for. It additionally pulls
#' out the datetime column, which accompanies any data for this class
#'
#' @param core_table a core table
#' @param input the input variable of choice
#' @param data_location the column name that stores the primary data for this
#' variable
#'
#' @return a long table with 1 row per event from the CC-HIC database
#' @export
#'
#' @importFrom rlang .data !! sym enquo
#' @importFrom magrittr %>%
#'
#' @examples
#' extract_2d(core, input = "NIHR_HIC_ICU_0108", data_location = "integer")
extract_2d <- function(core_table = NULL, input = NULL, data_location = NULL) {

  quo_column <- rlang::enquo(data_location)

  x <- core_table %>%
    filter(.data$code_name == input) %>%
    collect() %>%
    select(
      .data$event_id,
      .data$site,
      .data$code_name,
      .data$episode_id,
      .data$datetime,
      !!quo_column
    ) %>%
    rename(value = !!quo_column) %>%
    arrange(.data$episode_id, .data$datetime)

  correct_type <- qref[qref$code_name == input,"primary_column", drop = TRUE]
  if (any(stringr::str_detect(class(core_table), "SQLite")) & (
    correct_type %in% c("datetime", "date", "time")
  )) {
    y <- correct_type %>%
      base::switch(
        datetime = modify_datetime(extracted_table),
        date = modify_date(extracted_table),
        time = modify_time(extracted_table)
      )
    y <- mutate(y, datetime = lubridate::ymd_hms(datetime))
    return(y)
  }
  return(x)
}

modify_datetime <- function(x) {
  x <- mutate(x, value = lubridate::ymd_hms(.data$value))
}

modify_date <- function(x) {
  x <- mutate(x, value = lubridate::as_date(.data$value))
}

modify_time <- function(x) {
  x <- mutate(x, value = hms::as_hms(.data$value))
}
