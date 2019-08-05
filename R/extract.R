#' Extract NHIC Events
#'
#' Extracts NHIC events and appends them with the correct class for
#' further processing. This is essentially the creator method for
#' the S3 classes associated with the inspectEHR package
#'
#' @param core_table core table from make_core
#' @param input the input variable of choice
#'
#' @importFrom rlang .data !!
#' @importFrom magrittr %>%
#'
#' @return A tibble with 1 row per event
#' @export
#'
#' @examples
#' extract(core)
#' extract(core, input = "NIHR_HIC_ICU_0557")
extract <- function(core_table = NULL, input = "NIHR_HIC_ICU_0557") {

  # ensure the core table is provided
  if (is.null(core_table)) stop("You must include the core table")

  # Identify the correct column type to pull out
  dataitem <- qref %>%
    dplyr::filter(.data$code_name == input) %>%
    dplyr::select(.data$class) %>%
    dplyr::pull()

  # extract chosen input variable from the core table
  extracted_table <- dataitem %>%
    base::switch(
      integer_1d = extract_1d(core_table, input, data_location = "integer"),
      integer_2d = extract_2d(core_table, input, data_location = "integer"),
      real_1d = extract_1d(core_table, input, data_location = "real"),
      real_2d = extract_2d(core_table, input, data_location = "real"),
      string_1d = extract_1d(core_table, input, data_location = "string"),
      string_2d = extract_2d(core_table, input, data_location = "string"),
      datetime_1d = extract_1d(core_table, input, data_location = "datetime"),
      date_1d = extract_1d(core_table, input, data_location = "date"),
      time_1d = extract_1d(core_table, input, data_location = "time"))

  class(extracted_table) <- append(class(extracted_table), dataitem, after = 0)

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
#'
#' @examples
#' extract_1d(core, input = "NIHR_HIC_ICU_0409", data_location = "integer")
extract_1d <- function(core_table = NULL, input = NULL, data_location = NULL) {

  sym_code_name <- rlang::sym("code_name")
  quo_column <- enquo(data_location)

  extracted_table <- core_table %>%
    dplyr::filter(!! sym_code_name == input) %>%
    dplyr::collect() %>%
    dplyr::select(.data$event_id,
                  .data$site,
                  .data$code_name,
                  .data$episode_id,
                  !! quo_column) %>%
    dplyr::rename(value = !! quo_column,
                  internal_id = .data$event_id) %>%
    dplyr::arrange(episode_id)

  return(extracted_table)

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

  sym_code_name <- rlang::sym("code_name")
  quo_column <- rlang::enquo(data_location)

  extracted_table <- core_table %>%
    dplyr::filter(!! sym_code_name == input) %>%
    dplyr::collect() %>%
    dplyr::select(.data$event_id,
                  .data$site,
                  .data$code_name,
                  .data$episode_id,
                  .data$datetime,
                  !! quo_column) %>%
    dplyr::rename(value = !! quo_column,
                  internal_id = .data$event_id) %>%
    dplyr::arrange(.data$episode_id, .data$datetime)

  if (attributes(core_table$src$con)$class[1] == "SQLiteConnection") {

    extracted_table <- extracted_table %>%
      mutate(datetime = lubridate::as_datetime(datetime))

  }

  return(extracted_table)

}
