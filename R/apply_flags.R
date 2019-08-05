#' Apply all validation flags to an extracted data item
#'
#' Applies basic validation flags to an extracted data item.
#' This is a wrapper function with conditional logic to flag for:
#' data item out of value range
#' data item out of temporal range of the episode
#' duplication of item
#' periodicity
#'
#' periodicity checking is conditional on the preceding 3 flags,
#' as only those events that validate are taking into consideration
#' of periodicity checking
#'
#' @param x extracted data item
#' @param los_table episode length table
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
#'
#' @return a tibble with flags applied
#' @export
#'
#' @examples
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#' flag_all(hr, episode_length)
flag_all <- function(x, los_table = NULL) {

  # Aborts function if the class is not recognised
  if (!(any(class(x) %in% preserved_classes))) {
    stop("this function is not defined for this class")
  }

  # Captures NHIC input name
  input_name <- attr(x, "code_name")

  # Checks the availible methods for this class
  avail_methods <- methods(class = class(x)[1])
  event_class <- class(x)[1]

  # Apply RANGE FLAG if an appropriate method exists, or return NA
  if (any(grepl("flag_range", avail_methods))) {
    rf <- x %>% flag_range()
  } else {
    rf <- x %>%
      dplyr::mutate(range_error = NA) %>%
      dplyr::select(.data$internal_id, .data$range_error)
  }

  # Apply BOUNDARY FLAG if an appropriate method exists, or return NA
  if (any(grepl("flag_bounds", avail_methods))) {
    bf <- x %>% flag_bounds(los_table = los_table)
  } else {
    bf <- x %>%
      dplyr::mutate(out_of_bounds = NA) %>%
      dplyr::select(.data$internal_id, .data$out_of_bounds)
  }

  # Apply DUPLICATE FLAG if an appropriate method exists, or return NA
  if (any(grepl("flag_duplicate", avail_methods))) {
    df <- x %>% flag_duplicate()
  } else {
    df <- x %>%
      dplyr::mutate(duplicate = NA) %>%
      dplyr::select(.data$internal_id, .data$duplicate)
  }

  # Join the flags above back to the original df
  # This step must be performed prior to periodicity checking
  # Using LEFT join on the original table so as to not loose any data
  x %<>%
    left_join(rf, by = "internal_id") %>%
    left_join(bf, by = "internal_id") %>%
    left_join(df, by = "internal_id")

  # Apply PERIODICITY FLAG if an appropriate method exists, or return NA
  if (any(grepl("flag_periodicity", avail_methods))) {
    x %<>% flag_periodicity(los_table = los_table)
  } else {
    x %<>%
      dplyr::mutate(periodicity = NA)
  }

  attr(x, "code_name") <- input_name

  # Class tidying up
  if (any(class(x) %in% preserved_classes)) {

    return(x)

  } else {

    class(x) <- append(class(x), event_class, after = 0)
    return(x)

  }

}


#' Flag Events by Reference Range (S3 Generic)
#'
#' Flags each event as being in-range (0), out-of-range high (+1) or
#' out-of-range low (-1). These ranges have been calibrated by hand based on
#' prior evidence or expert opinion. Reference ranges are all found in
#' \code{\link{qref}}.
#'
#' @param x an extracted event table
#' @param ... further arguments to pass to the S3 method
#'
#' @return a tibble of the same length as x with the following features:
#' \describe{
#'   \item{-1}{event is below the defined range of plausibility}
#'   \item{0}{event is valid}
#'   \item{+1}{event is above the defined range of plausibility}
#' }
#'
#' @export
#'
#' @examples
#' flag_range(x)
flag_range <- function(x = NULL) {
  UseMethod("flag_range", x)
}


flag_range.default <- function(...) {
  print("there are no methods for this class")
}


#' Flag Range Numeric
#'
#' The generic function for numeric type range flags
#' Retuns 103 if value too low, 104 if value too high, and NA if
#' there is no reference for ranges
#'
#' @param x
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data !!
#' @example
#' flag_range_numeric(df)
flag_range_numeric <- function(x = NULL) {

  # joins to the quality refernce table to identify range errors
  x <- x %>%
    dplyr::left_join(qref, by = "code_name") %>%
    dplyr::mutate(
      range_error = if_else(.data$value > .data$range_max, 104L,
                    if_else(.data$value < .data$range_min, 103L, 0L))) %>%
    dplyr::select(.data$internal_id, .data$range_error)

  return(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
flag_range.real_2d <- function(x = NULL) {

  x %<>% flag_range_numeric()
  class(x) <- append(class(x), "real_2d", after = 0)
  return(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
flag_range.real_1d <- function(x = NULL) {

  x %<>% flag_range_numeric()
  class(x) <- append(class(x), "real_1d", after = 0)
  return(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
flag_range.integer_2d <- function(x = NULL) {

  x %<>% flag_range_numeric()
  class(x) <- append(class(x), "integer_2d", after = 0)
  return(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
flag_range.integer_1d <- function(x = NULL) {

  x %<>% flag_range_numeric()
  class(x) <- append(class(x), "integer_1d", after = 0)
  return(x)

}


#' Flag Range String
#'
#' Returns 105 if string value is outside enumerated list
#'
#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
#' @importFrom stringr str_length
flag_range.string_2d <- function(x = NULL) {

  # note, there are only 2 strings in the db that are 2d -> bugs and airway

  # Checks to see if this is microdata, and if so aborts
  if (attr(x, "code_name") == "NIHR_HIC_ICU_0187") {

    x %<>%
      dplyr::select(.data$internal_id) %>%
      dplyr::mutate(range_error = 0L)

  } else {

  # The only other possibility is airway data
    x %<>%
      dplyr::mutate(
        range_error = ifelse(value %in% c("E", "N", "T"), 0L, 105L)) %>%
      dplyr::select(.data$internal_id, .data$range_error)

  }

  class(x) <- append(class(x), "string_2d", after = 0)
  return(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data !!
#' @importFrom tidyr unnest
flag_range.string_1d <- function(x = NULL) {

  flags_applied <- FALSE

  code_name <- attr(x, "code_name")
  quo_codename <- enquo(code_name)

  row_count <- qref %>%
    filter(.data$code_name == !! quo_codename,
           .data$possible_values != "NULL") %>%
    nrow()

  if (row_count == 1) {

    # This handles the majority of string enumerated cases

    possible_values <- qref %>%
      filter(.data$code_name == !! quo_codename) %>%
      select(.data$possible_values) %>%
      unnest() %>%
      select(.data$possible_values) %>%
      pull()

    x <- x %>%
      dplyr::mutate(
        range_error = if_else(
          is.na(.data$value), as.integer(NA),
          if_else(.data$value %in% possible_values, 0L, 105L))) %>%
      dplyr::select(.data$internal_id, .data$range_error)

    flags_applied <- TRUE

  } else {

    # We need to check a few special cases here

    if (code_name == "NIHR_HIC_ICU_0076") {
      # NIHR_HIC_ICU_0076 - Post code
      # This evaluates for full post code only

      x <- x %>%
        dplyr::mutate(
          range_error = if_else(validate_post_code(.data$value), 0L, 109L)) %>%
        dplyr::select(.data$internal_id, .data$range_error)

      flags_applied <- TRUE

    }

    if (code_name == "NIHR_HIC_ICU_0073") {
      # NIHR_HIC_ICU_0073 - NHS Number

      x <- x %>%
        dplyr::mutate(
          range_error = if_else(is.na(.data$value), as.integer(NA),
                                if_else(validate_nhs(.data$value), 0L, 109L))) %>%
        dplyr::select(.data$internal_id, .data$range_error)

      flags_applied <- TRUE

    }

    if (code_name %in% c("NIHR_HIC_ICU_0399", "NIHR_HIC_ICU_0088", "NIHR_HIC_ICU_0912")) {
      # NIHR_HIC_ICU_0399 - Primary Admission Reason
      # NIHR_HIC_ICU_0088 - Secondary Admission Reason
      # NIHR_HIC_ICU_0912 - Ultimate Primary
      # This doesn't handle partial codes yet

      x <- x %>%
        dplyr::mutate(
          range_error = ifelse(is.na(.data$value), as.integer(NA),
                               if_else(validate_icnarc(.data$value), 0L, 109L))) %>%
        dplyr::select(.data$internal_id, .data$range_error)

      flags_applied <- TRUE

    }

    # Others not yet covered
    # NIHR_HIC_ICU_0074 - Other Conditions in PMHx

  }

  if (!flags_applied) {

    x <- x %>%
      dplyr::mutate(
        range_error = as.integer(NA)) %>%
      dplyr::select(.data$internal_id, .data$range_error)

  }

  class(x) <- append(class(x), "string_1d", after = 0)

  return(x)

}


#' @export
#' @importFrom lubridate dmy
flag_range.date_1d <- function(x = NULL) {

  # These are very primitive checks for date
  x %<>%
    dplyr::mutate(
      range_error = ifelse(.data$value > Sys.Date(), 104L,
                    ifelse(.data$value < lubridate::dmy("01/01/1900"), 103L, 0))) %>%
    dplyr::select(.data$internal_id, .data$range_error)

  class(x) <- append(class(x), "date_1d", after = 0)

  return(x)

}


#' @export
#' @importFrom lubridate dmy_hms
flag_range.datetime_1d <- function(x = NULL) {

  # These are very primitive checks for datetime
  x %<>%
    dplyr::mutate(
      range_error = ifelse(.data$value > Sys.time(), 104L,
                    ifelse(.data$value < lubridate::dmy_hms(
                      "01/01/1900 00:00:00"),
                  103L, 0))) %>%
    dplyr::select(.data$internal_id, .data$range_error)

  class(x) <- append(class(x), "datetime_1d", after = 0)

  return(x)

}


# ==== FLAG BY EPISODE BOUNDARIES


#' Flag Events by Episode Boundaries (S3 Generic)
#'
#' If the event of concern is a string, since only Airway and Organism are time
#' varying string fields the behavior on hic_str differs.
#'
#' @param x an extracted tibble
#' @param ...
#'
#' @return a tibble of the same length as x with the following values
#' \describe{
#'   \item{-1}{event occurred significantly before ICU episode}
#'   \item{0}{event occurred during ICU episode}
#'   \item{+1}{event occurred significantly after ICU episode}
#'   \item{NA}{Foundation elements were not availible and so this parameter could not be calculated}
#' }
#' @export
#'
#' @examples
#'
#' flag_bounds(x, los_table)
flag_bounds <- function(x, los_table = NULL) {
  UseMethod("flag_bounds", x)
}


flag_bounds.default <- function(...) {

  print("there are no methods for this class")

}


#' Flag boundaries of 2d data items
#'
#' Sets boundary conditions for events in relation to episodes
#'
#' @param x an extracted nhic event table
#' @param los_table episode length table
#'
#' @return a two column tibble with internal id and duplication status
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data enquo
#' @export
#'
#' @examples
#' flag_bounds_2d(x, los_table)
flag_bounds_2d <- function(x = NULL, los_table = NULL) {

  x <- x %>%
    left_join(los_table %>%
                select(-.data$site), by = "episode_id") %>%
    mutate(out_of_bounds = if_else(
      (.data$validity != 0L), as.integer(NA),
        if_else(
      (as.integer(difftime(.data$datetime, .data$epi_start_dttm, units = "days")) < -2L), 101L,
        if_else(
      (as.integer(difftime(.data$datetime, .data$epi_end_dttm, units = "days")) > 2L), 102L, 0L)))) %>%
    select(.data$internal_id, .data$out_of_bounds)

  return(x)

}


#' @export
flag_bounds.real_2d <- function(x = NULL, los_table = NULL) {

  x %<>% flag_bounds_2d(los_table = los_table)

  class(x) <- append(class(x), "real_2d", after = 0)

  return(x)

}


#' @export
flag_bounds.integer_2d <- function(x = NULL, los_table = NULL) {

  x %<>% flag_bounds_2d(los_table = los_table)

  class(x) <- append(class(x), "integer_2d", after = 0)

  return(x)

}


#' @export
flag_bounds.string_2d <- function(x = NULL, los_table = NULL) {

  x %<>% flag_bounds_2d(los_table = los_table)

  class(x) <- append(class(x), "string_2d", after = 0)

  return(x)

}


# ==== FLAG DUPLICATE


#' Flag Duplicates (S3 Generic)
#'
#' Flags nhic events for duplication. Duplicate events are assigned the value 1L
#'
#' @param x an extracted nhic event table
#' @param ...
#'
#' @return a two column tibble with internal id and duplication status
#' @export
#'
#' @examples
#' flag_duplicate(x)
flag_duplicate <- function(x) {
  UseMethod("flag_duplicate", x)
}


flag_duplicate.default <- function(...) {

  print("no default methods are defined for this class")

}


#' @export
flag_duplicate_2d <- function(x = NULL) {

  x %<>%
    ungroup() %>%
    distinct(.data$episode_id,
             .data$datetime,
             .data$value,
             .keep_all = TRUE) %>%
    mutate(duplicate = 0L) %>%
    select(.data$internal_id, .data$duplicate) %>%
    right_join(x, by = "internal_id") %>%
    mutate_at(.vars = vars(.data$duplicate),
              .funs = funs(ifelse(is.na(.), 106L, .))) %>%
    select(.data$internal_id, .data$duplicate)

  return(x)

}


#' @export
flag_duplicate.real_2d <- function(x = NULL) {

  x %<>% flag_duplicate_2d()

  class(x) <- append(class(x), "real_2d", after = 0)

  return(x)

}


#' @export
flag_duplicate.integer_2d <- function(x = NULL) {

  x %<>% flag_duplicate_2d()

  class(x) <- append(class(x), "integer_2d", after = 0)

  return(x)

}


#' @export
flag_duplicate.string_2d <- function(x = NULL) {

  x %<>% flag_duplicate_2d()

  class(x) <- append(class(x), "string_2d", after = 0)

  return(x)

}


#' @export
flag_duplicate_1d <- function(x = NULL) {

  x %<>%
    ungroup() %>%
    distinct(.data$episode_id,
             .data$value,
             .keep_all = TRUE) %>%
    mutate(duplicate = 0L) %>%
    select(.data$internal_id, .data$duplicate) %>%
    right_join(x, by = "internal_id") %>%
    mutate_at(.vars = vars(.data$duplicate),
              .funs = funs(ifelse(is.na(.), 106L, .))) %>%
    select(.data$internal_id, .data$duplicate)

  return(x)

}


#' @export
flag_duplicate.real_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "real_1d", after = 0)

  return(x)

}


#' @export
flag_duplicate.integer_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "integer_1d", after = 0)

  return(x)

}




#' @export
flag_duplicate.string_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "string_1d", after = 0)

  return(x)

}


#' @export
flag_duplicate.datetime_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "datetime_1d", after = 0)

  return(x)

}


#' @export
flag_duplicate.date_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "date_1d", after = 0)

  return(x)

}


#' @export
flag_duplicate.time_1d <- function(x = NULL) {

  x %<>% flag_duplicate_1d()

  class(x) <- append(class(x), "time_1d", after = 0)

  return(x)

}

#' Flags for Periodicity of a Data Item (S3 Generic)
#'
#' Takes a data item tibble that has been pre-flagged with range, boundary and duplicate errors
#' and provides a column indicating the periodicity of that data item stratified by each episode
#' number. This is a similar concept to the coverage mapping, although is looking at patient
#' level rather than site level.
#'
#' These are only defined for 2d data
#'
#' Periodicity is given as event count per los unit. The default behaviour will be events/day.
#'
#' @param x an extracted nhic events table
#' @param los_table episode length table
#'
#' @return an extension of x with a mutated periodicity column
#' @export
#'
#' @examples
#' flag_periodicity(x, los_table)
flag_periodicity <- function(x, los_table = NULL) {
  UseMethod("flag_periodicity", x)
}


flag_periodicity.default <- function(x) {

  print("There are no default methods for this class")

}


#' Flag periodicity of 2d class
#'
#' @param x data item
#' @param los_table episode length table
#'
#' @return a mutated tibble from x including a periodicity column
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang .data
#'
#' @examples
#' flag_periodicity(x, los_table)
flag_periodicity_generic <- function(x, los_table = NULL) {

  x %<>%

    # filter out values that cannot be taken into consideration for this calculation
    dplyr::filter(.data$out_of_bounds == 0L,
                  .data$range_error == 0L,
                  .data$duplicate == 0L) %>%

    # only need 1 value of interest to track periodicity (we'll choose datetime)
    dplyr::select(.data$episode_id, .data$datetime) %>%
    dplyr::group_by(.data$episode_id) %>%

    # count the number of events occurring
    dplyr::summarise(count = n()) %>%
    dplyr::left_join(los_table %>%

                       # only checking validated episodes
                       dplyr::filter(.data$validity == 0L) %>%
                       dplyr::select(.data$episode_id, .data$los),
                     by = "episode_id") %>%

    # calculate the periodicity
    dplyr::mutate(periodicity = count/as.numeric(los)) %>%
    dplyr::select(.data$episode_id, .data$periodicity) %>%

    # right join back into the original object
    # this will produce NAs on the following conditions: invalid LOS or no usable values
    # NAs are also produced if the test is inappropraite i.e. 1d data
    dplyr::right_join(x, by = "episode_id")

  return(x)

}


#' @export
flag_periodicity.real_2d <- function(x, los_table = NULL) {

  x <- flag_periodicity_generic(x, los_table = los_table)

  class(x) <- append(class(x), "real_2d", after = 0)

  return(x)

}


#' @export
flag_periodicity.integer_2d <- function(x, los_table = NULL) {

  x <- flag_periodicity_generic(x, los_table = los_table)

  class(x) <- append(class(x), "integer_2d", after = 0)

  return(x)

}


#' @export
flag_periodicity.string_2d <- function(x, los_table = NULL) {

  x <- flag_periodicity_generic(x, los_table = los_table)

  class(x) <- append(class(x), "string_2d", after = 0)

  return(x)

}


#' Flag long-term typical contribution
#'
