#' Verify CC-HIC Data
#'
#' Applies all relevent varification flags to an extracted dataitem. This
#' includes:
#' \itemize{
#'   \item Completeness: contributed data items match local capability (i.e.
#'     missingness only occurs when the data doesn't exist) Performed by
#'     \code{\link{verify_complete}}
#'   \item Uniqueness plausibility: descriptions of singular events/objects are
#'     not duplicated. Performed by \code{\link{verify_unique_plausible}}
#'   \item Atemporal plausibility: Events occur within their episode (within a
#'     reasonable buffer time). Events fall within an accepted range, follow the
#'     expected distribution and agree with internal/local knowledge. Repeated
#'     measures of the same event show the expected variability. Performed by
#'     \code{\link{verify_atemporal_plausible}}
#'   \item Temporal plausibility: value density over time are consistent with
#'     local expectations
#'     }
#'
#' Other varification components are found elsewhere, as they don't necessarily
#' fit into an evaludation at the data item level. I am contemplating how to
#' unify this procedure.
#'
#' @param x extracted dataitem from \code{\link{extract}}
#' @param los_table episode length table from \code{\link{characterise_episodes}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data !!
#'
#' @return a tibble with verification applied
#' @export
#'
#' @examples
#' ## DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#' episode_length <- characterise_episodes(ctn)
#' ve <- verify_episodes(episode_length)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## Full varification
#' vhr <- verify_events(hr, ve)
#' head(vhr)
#' DBI::dbDisconnect(ctn)
verify_events <- function(x, los_table = NULL) {

  if (is.null(los_table)) rlang::abort("You must supply an episode table")

  # Aborts function if the class is not recognised
  if (!(any(class(x)[1] %in% preserved_classes))) {
    rlang::abort("this function is not defined for this class")
  }

  # Captures the input code_name
  input_name <- attr(x, "code_name")

  # Check availible methods for this class
  avail_methods <- methods(class = class(x)[1])
  event_class <- class(x)[1]

  # Range Verification
  if (any(grepl("verify_range", avail_methods))) {
    rf <- verify_range(x)
  } else {
    rf <- x %>%
      mutate(range_error = as.integer(NA)) %>%
      select(.data$event_id, .data$range_error)
  }

  # Boundary Verification
  if (any(grepl("verify_bounds", avail_methods))) {
    bf <- verify_bounds(x, los_table = los_table)
  } else {
    bf <- x %>%
      mutate(out_of_bounds = as.integer(NA)) %>%
      select(.data$event_id, .data$out_of_bounds)
  }

  # Duplicate Verification
  if (any(grepl("verify_duplicate", avail_methods))) {
    df <- verify_duplicate(x)
  } else {
    df <- x %>%
      mutate(duplicate = as.integer(NA)) %>%
      select(.data$event_id, .data$duplicate)
  }

  # Join the labels above back to the original df
  # This step must be performed PRIOR to periodicity checking
  x <- x %>%
    left_join(rf, by = "event_id") %>%
    left_join(bf, by = "event_id") %>%
    left_join(df, by = "event_id")

  # Periodicity Verification
  if (any(grepl("verify_periodicity", avail_methods))) {
    x <- verify_periodicity(x, los_table = los_table)
  } else {
    x <- x %>% mutate(periodicity = as.numeric(NA),
                      var_per = as.integer(NA))
  }

  attr(x, "code_name") <- input_name

  # Class tidying up
  if (any(class(x) %in% preserved_classes)) {
    class(x) <- append(class(x), "varified", after = 0)
    return(x)
  } else {
    class(x) <- append(class(x), event_class, after = 0)
    class(x) <- append(class(x), "varified", after = 0)
    return(x)
  }
}


#' verify Event Plausibility - Range Checks (S3 Generic)
#'
#' Varifies events as being in-range (\code{0}), high (\code{+1}) or
#' low (\code{-1}). These ranges have been calibrated based upone; ICNARC
#' reference ranges, prior evidence (usually case report for exceptional values)
#' or expert opinion. Reference ranges are all found in \code{\link{qref}}.
#'
#' @param x an extracted event table
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
#' ## DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## verify Range
#' vhr <- verify_range(hr)
#' head(vhr)
#' DBI::dbDisconnect(ctn)
verify_range <- function(x = NULL) {
  if (is.null(x)) {
    rlang::abort("You must supply an extract data item")
  }
  UseMethod("verify_range", x)
}

#' @export
#' @importFrom rlang abort
verify_range.default <- function(...) {
  rlang::abort("There are no methods defined for this data type")
}


#' Range Checks - Numeric
#'
#' The generic function for numeric type range verification.
#'
#' @param x an extracted table
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate case_when select
#' @importFrom rlang .data
verify_range_numeric <- function(x = NULL) {
  x <- x %>%
    left_join(qref, by = "code_name") %>%
    mutate(
      range_error = case_when(
        .data$value > .data$range_max ~ 1L,
        .data$value < .data$range_min ~ -1L,
        TRUE ~ 0L
      )
    ) %>%
    select(.data$event_id, .data$range_error)
}

#' @export
verify_range.real_2d <- function(x = NULL) {
  x <- verify_range_numeric(x)
  if (!("real_2d" %in% class(x))) {
    class(x) <- append(class(x), "real_2d", after = 0)
  }
  return(x)
}

#' @export
verify_range.real_1d <- function(x = NULL) {
  x <- verify_range_numeric(x)
  if (!("real_1d" %in% class(x))) {
    class(x) <- append(class(x), "real_1d", after = 0)
  }
  return(x)
}

#' @export
verify_range.integer_2d <- function(x = NULL) {
  x <- verify_range_numeric(x)
  if (!("integer_2d" %in% class(x))) {
    class(x) <- append(class(x), "integer_2d", after = 0)
  }
  return(x)
}

#' @export
verify_range.integer_1d <- function(x = NULL) {
  x <- verify_range_numeric(x)
  if (!("integer_1d" %in% class(x))) {
    class(x) <- append(class(x), "integer_1d", after = 0)
  }
  return(x)
}

#' Range Checks - String
#'
#' The generic function for string type range verification.
#'
#' @param x an extracted table
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! enquo
#' @importFrom tidyr unnest
verify_range_string <- function(x = NULL) {

  flags_applied <- FALSE
  code_name <- attr(x, "code_name")
  quo_codename <- enquo(code_name)

  # Check to see if we have a solution in the dq_ref
  solutions <- qref %>%
    filter(
      .data$code_name == !!quo_codename,
      !is.null(.data$possible_values)
    ) %>%
    pull() %>%
    unlist()

  if (!is.null(solutions)) {

    # This handles the majority of string enumerated cases
    possible_values <- qref %>%
      filter(.data$code_name == !!quo_codename) %>%
      select(.data$possible_values) %>%
      unnest() %>%
      select(.data$possible_values) %>%
      pull()

    x <- x %>%
      mutate(
        range_error = case_when(
          is.na(.data$value) ~ as.integer(NA),
          .data$value %in% possible_values ~ 0L,
          TRUE ~ 1L
        )
      ) %>%
      select(.data$event_id, .data$range_error)

    flags_applied <- TRUE

  } else {

    if (code_name == "NIHR_HIC_ICU_0076") {
      # NIHR_HIC_ICU_0076 - Post code
      # This evaluates for full post code only

      x <- x %>%
        mutate(
          range_error = case_when(
            is.na(.data$value) ~ as.integer(NA),
            verify_post_code(.data$value) ~ 0L,
            TRUE ~ 1L
          )
        ) %>%
        select(.data$event_id, .data$range_error)

      flags_applied <- TRUE
    }

    if (code_name == "NIHR_HIC_ICU_0073") {
      # NIHR_HIC_ICU_0073 - NHS Number

      x <- x %>%
        dplyr::mutate(
          range_error = case_when(
            is.na(.data$value) ~ as.integer(NA),
            verify_nhs(.data$value) ~ 0L,
            TRUE ~ 1L
          )
        ) %>%
        dplyr::select(.data$event_id, .data$range_error)

      flags_applied <- TRUE
    }

    if (code_name %in% c("NIHR_HIC_ICU_0399",
                         "NIHR_HIC_ICU_0088",
                         "NIHR_HIC_ICU_0912",
                         "NIHR_HIC_ICU_0074")) {
      # NIHR_HIC_ICU_0399 - Primary Admission Reason
      # NIHR_HIC_ICU_0088 - Secondary Admission Reason
      # NIHR_HIC_ICU_0912 - Ultimate Primary
      # NIHR_HIC_ICU_0074 - Other Conditions in PMHx
      # This doesn't handle partial codes yet

      x <- x %>%
        dplyr::mutate(
          range_error = case_when(
            is.na(.data$value) ~ as.integer(NA),
            verify_icnarc(.data$value) ~ 0L,
            TRUE ~ 1L
          )
        ) %>%
        dplyr::select(.data$event_id, .data$range_error)

      flags_applied <- TRUE
    }

  }

  if (!flags_applied) {
    x <- x %>%
      dplyr::mutate(
        range_error = as.integer(NA)
      ) %>%
      dplyr::select(.data$event_id, .data$range_error)
  }


  return(x)
}

#' @export
verify_range.string_2d <- function(x = NULL) {
  x <- verify_range_string(x)
  if (!("string_2d" %in% class(x))) {
    class(x) <- append(class(x), "string_2d", after = 0)
  }
  return(x)
}

#' @export
verify_range.string_1d <- function(x = NULL) {
  x <- verify_range_string(x)
  if (!("string_1d" %in% class(x))) {
    class(x) <- append(class(x), "string_1d", after = 0)
  }
  return(x)

}


#' Range Checks - Dates
#'
#' Verifies that all dates are before now (i.e. not in the future) and not
#' before 1900-01-01, which seems reasonable.
#'
#' @param x an extracted table
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
verify_range.date_1d <- function(x = NULL) {

  x <- x %>%
    mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.integer(NA),
        .data$value > Sys.Date() ~ 1L,
        .data$value < as.Date("1900-01-01") ~ -1L,
        TRUE ~ 0L
      )
    ) %>%
    select(.data$event_id, .data$range_error)

  if (!("date_1d" %in% class(x))) {
    class(x) <- append(class(x), "date_1d", after = 0)
  }

  return(x)
}

#' Range Checks - Times
#'
#' Verifies that all times are between 00:00:00 and 23:59:59
#'
#' @param x an extracted table
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
#' @importFrom hms as_hms
verify_range.time_1d <- function(x = NULL) {

  x <- x %>%
    mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.integer(NA),
        .data$value > as_hms("23:59:59") ~ 1L,
        .data$value < as_hms("00:00:00") ~ -1L,
        TRUE ~ 0L
      )
    ) %>%
    select(.data$event_id, .data$range_error)

  if (!("time_1d" %in% class(x))) {
    class(x) <- append(class(x), "time_1d", after = 0)
  }

  return(x)
}


#' Range Checks - Datetimes
#'
#' Verifies that all dates are before now (i.e. not in the future) and not
#' before 1900-01-01 00:00:00, which seems reasonable.
#'
#' @param x an extracted table
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
verify_range.datetime_1d <- function(x = NULL) {

  x <- x %>%
    mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.integer(NA),
        .data$value > Sys.time() ~ 1L,
        .data$value < as.POSIXct("1900-01-01 00:00:00") ~ -1L,
        TRUE ~ 0L
      )
    ) %>%
    select(.data$event_id, .data$range_error)

  if (!("datetime_1d" %in% class(x))) {
    class(x) <- append(class(x), "datetime_1d", after = 0)
  }

  return(x)
}


#' verify Event Plausibility - Boundary Checks (S3 Generic)
#'
#' Varifies events as being within (\code{0}), after (\code{+1}) or
#' before (\code{-1}) an associated episode. This isn't necessarily a problem,
#' for example, microbiology data coming back after death. However, it is
#' often demonstrative of a bigger problem. For example, data is sometimes
#' contributes from more than one episode, and then duplicated across episodes.
#'
#' @param x an extracted nhic event table
#' @param los_table episode length table
#' @param hours the number of hours you allow before and after an episode
#'
#' @return a tibble of the same length as x with the following features:
#' \describe{
#'   \item{-1}{event is before the ICU episode}
#'   \item{0}{event is during the ICU episode}
#'   \item{+1}{event is after the ICU episode}
#' }
#'
#' @export
#' @importFrom rlang abort
#'
#' @examples
#' ## DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#' episodes <- characterise_episodes(ctn)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## verify Boundary Conditions
#' vhr <- verify_bounds(hr, los_table = episodes)
#' head(vhr)
#' DBI::dbDisconnect(ctn)
verify_bounds <- function(x, ...) {
  UseMethod("verify_bounds", x)
}

#' @export
#' @importFrom rlang abort
verify_bounds.default <- function(...) {
  rlang::abort("There are no methods defined for this data type")
}


#' verify boundaries of 2d data items
#'
#' @param x an extracted nhic event table
#' @param los_table episode length table
#' @param hours the number of hours you allow before and after an episode
#' @export
#' @return a two column tibble with event id and boundary status
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr left_join select mutate case_when
verify_bounds_2d <- function(x = NULL, los_table = NULL, hours = 24) {
  if (is.null(los_table)) abort("You must supply an episode length table.")

  los_table <- los_table %>%
    select(.data$episode_id,
           .data$epi_start_dttm,
           .data$epi_end_dttm)

  x <- x %>%
    left_join(los_table, by = "episode_id") %>%
    mutate(out_of_bounds = case_when(
        as.integer(
          difftime(
            .data$datetime, .data$epi_start_dttm, units = "hours")
          ) < hours ~ -1L,
        as.integer(
          difftime(
            .data$datetime, .data$epi_end_dttm, units = "hours")
          ) > hours ~ 1L,
        TRUE ~ 0L
        )
      ) %>%
    select(.data$event_id, .data$out_of_bounds)

  return(x)
}

#' @export
verify_bounds.real_2d <- function(x, los_table, ...) {
  x <- verify_bounds_2d(x, los_table = los_table)
  if (!("real_2d" %in% class(x))) {
    class(x) <- append(class(x), "real_2d", after = 0)
  }
  return(x)
}

#' @export
verify_bounds.integer_2d <- function(x, los_table, ...) {
  x <- verify_bounds_2d(x, los_table = los_table)
  if (!("integer_2d" %in% class(x))) {
    class(x) <- append(class(x), "integer_2d", after = 0)
  }
  return(x)
}

#' @export
verify_bounds.string_2d <- function(x, los_table, ...) {
  x <- verify_bounds_2d(x, los_table = los_table)
  if (!("string_2d" %in% class(x))) {
    class(x) <- append(class(x), "string_2d", after = 0)
  }
  return(x)
}

#' verify Event Plausibility - Duplication Checks (S3 Generic)
#'
#' Varifies events as being duplicated (\code{+1}) or not. This is primarily
#' trained on the datetime of an object (if 2d), and looks for two events that
#' are perfectly co-incident. This assumptions can be relaxed with
#' \code{exact = FALSE} which looks to see if there are any duplicates in
#' value for the preceeding/following 12 hours. This is pointless for dataitems
#' like heart rate, but is useful so dataitems like creatinine, where we have
#' seen this type of duplication error. 1d events are checked for raw
#' duplication, since they should only occur once per episode anyway.
#'
#' @param x an extracted event table
#' @param exact TRUE/FALSE
#'
#' @return a tibble of the same length as x with the following features:
#' \describe{
#'   \item{+1}{value is a suspected/confirmed duplicate}
#'   \item{0}{event is unique}
#' }
#'
#' Note, when 2 values are deems to be duplicates, the one that first appears
#' in the database is cleared as verified, while the second is not.
#'
#' @export
#'
#' @examples
#' ## DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## verify Range
#' vhr <- verify_duplicate(hr)
#' head(vhr)
#' DBI::dbDisconnect(ctn)
verify_duplicate <- function(x, exact = TRUE) {
  UseMethod("verify_duplicate", x)
}

#' @export
#' @importFrom rlang abort
verify_duplicate.default <- function(...) {
  rlang::abort("There are no methods defined for this data type")
}

#' @export
#' @importFrom dplyr ungroup distinct mutate select right_join mutate_at if_else
#' @importFrom rlang .data
#' @importFrom lubridate round_date
verify_duplicate_2d <- function(x = NULL, exact = TRUE) {
  if (exact == TRUE) {
    x <- x %>%
      ungroup() %>%
      distinct(
        .data$episode_id,
        .data$datetime,
        .data$value,
        .keep_all = TRUE
      ) %>%
      mutate(duplicate = 0L) %>%
      select(.data$event_id, .data$duplicate) %>%
      right_join(x, by = "event_id") %>%
      mutate_at(
        .vars = vars(.data$duplicate),
        .funs = function(x) if_else(is.na(x), 1L, x)
      ) %>%
      select(.data$event_id, .data$duplicate)
    return(x)
  } else {
    x <- x %>%
      ungroup() %>%
      mutate(
        datetime = lubridate::round_date(datetime, unit = "12 hours")
        ) %>%
      distinct(
        .data$episode_id,
        .data$datetime,
        .data$value,
        .keep_all = TRUE
        ) %>%
      mutate(duplicate = 0L) %>%
      select(event_id, duplicate) %>%
      right_join(x, by = "event_id") %>%
      mutate_at(
        .vars = vars(duplicate),
        .funs = function(x) if_else(is.na(x), 1L, x)
      ) %>%
      select(.data$event_id, .data$duplicate)
    return(x)
  }
}

#' @export
verify_duplicate.real_2d <- function(x = NULL, ...) {
  x <- verify_duplicate_2d(x, ...)
  if (!("real_2d" %in% class(x))) {
    class(x) <- append(class(x), "real_2d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.integer_2d <- function(x = NULL, ...) {
  x <- verify_duplicate_2d(x, ...)
  if (!("integer_2d" %in% class(x))) {
    class(x) <- append(class(x), "integer_2d", after = 0)
  }
  return(x)
}


#' @export
verify_duplicate.string_2d <- function(x = NULL, ...) {
  x <- verify_duplicate_2d(x, ...)
  if (!("string_2d" %in% class(x))) {
    class(x) <- append(class(x), "string_2d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate_1d <- function(x = NULL) {
  x <- x %>%
    ungroup() %>%
    distinct(
      .data$episode_id,
      .data$value,
      .keep_all = TRUE
    ) %>%
    mutate(duplicate = 0L) %>%
    select(.data$event_id, .data$duplicate) %>%
    right_join(x, by = "event_id") %>%
    mutate_at(
      .vars = vars(.data$duplicate),
      .funs = function(x) if_else(is.na(x), 1L, x)
    ) %>%
    select(.data$event_id, .data$duplicate)

  return(x)
}

#' @export
verify_duplicate.real_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("real_1d" %in% class(x))) {
    class(x) <- append(class(x), "real_1d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.integer_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("integer_1d" %in% class(x))) {
    class(x) <- append(class(x), "integer_1d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.string_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("string_1d" %in% class(x))) {
    class(x) <- append(class(x), "string_1d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.datetime_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("datetime_1d" %in% class(x))) {
    class(x) <- append(class(x), "datetime_1d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.date_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("date_1d" %in% class(x))) {
    class(x) <- append(class(x), "date_1d", after = 0)
  }
  return(x)
}

#' @export
verify_duplicate.time_1d <- function(x = NULL) {
  x <- verify_duplicate_1d(x)
  if (!("time_1d" %in% class(x))) {
    class(x) <- append(class(x), "time_1d", after = 0)
  }
  return(x)
}

#' verify Event Plausibility - Periodicity Checks (S3 Generic)
#'
#' Provides the periodicity of an extracted data item. This is the number of
#' submitted varified values per day. As such, the range, boundary and
#' duplication varification methods must have been first run. Periodicity is
#' only defined for 2d data.
#'
#' As periodicity for dataitems is not yet specified, this highlights cases
#' that are below the group 5th centile \code{-1} and above the 95th centile
#' \code{+1}
#'
#' This is a similar concept to the \code{\link{verify_coverage}}, although is
#' concerned with the episode, rather than site, level.
#'
#' @param x an extracted events table
#' @param los_table episode length table
#'
#' @return x with a periodicity value and validation columns
#' @export
verify_periodicity <- function(x, ...) {
  UseMethod("verify_periodicity", x)
}

#' @export
verify_periodicity.default <- function(...) {
  rlang::abort("There are no default methods for this class")
}


#' verify periodicity of 2d class
#'
#' @param x extracted dataitem
#' @param los_table episode length table
#'
#' @return a mutated tibble from x including a periodicity column
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr intersect filter select group_by left_join mutate right_join tally
verify_periodicity_generic <- function(x, los_table) {
  name_check <- dplyr::intersect(
    names(x),
    c("out_of_bounds", "range_error", "duplicate")
    )

  if (length(name_check) != 3) {
    rlang::abort("You must supply a dataframe to `x` that contains columns with
                 names `out_of_bounds`, `range_error` and `duplicate`")
  }
  x <- x %>%
    # filter out values that cannot be taken into consideration for this
    # calculation
    filter(
      .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
      .data$range_error == 0L | is.na(.data$range_error),
      .data$duplicate == 0L | is.na(.data$duplicate)
    ) %>%
    group_by(.data$episode_id) %>%
    tally() %>%
    left_join(los_table %>%
      # only checking validated episodes
      filter(.data$veracity == 0L) %>%
      select(.data$episode_id, .data$los_days),
    by = "episode_id"
    ) %>%
    # calculate the periodicity
    mutate(periodicity = n / as.numeric(los_days)) %>%
    select(.data$episode_id, .data$periodicity) %>%

    # right join back into the original object
    # this will produce NAs on the following conditions: invalid LOS or no
    # usable values
    right_join(x, by = "episode_id")

  quan <- quantile(x$periodicity,
                   na.rm = TRUE,
                   probs = c(0.05, 0.95),
                   names = FALSE)

  x <- x %>%
    mutate(var_per = case_when(
      is.na(periodicity) ~ as.integer(NA),
      periodicity < quan[1] ~ -1L,
      periodicity > quan[2] ~ 1L,
      TRUE ~ 0L
    ))
}

#' @export
verify_periodicity.real_2d <- function(x, los_table, ...) {
  x <- verify_periodicity_generic(x, los_table = los_table)
  if (!("real_2d" %in% class(x))) {
    class(x) <- append(class(x), "real_2d", after = 0)
  }
  return(x)
}

#' @export
verify_periodicity.integer_2d <- function(x, los_table, ...) {
  x <- verify_periodicity_generic(x, los_table = los_table)
  if (!("integer_2d" %in% class(x))) {
    class(x) <- append(class(x), "integer_2d", after = 0)
  }
  return(x)
}

#' @export
verify_periodicity.string_2d <- function(x, los_table, ...) {
  x <- verify_periodicity_generic(x, los_table = los_table)
  if (!("string_2d" %in% class(x))) {
    class(x) <- append(class(x), "string_2d", after = 0)
  }
  return(x)
}

#' verify Event Plausibility - Coverage Checks (S3 Generic)
#'
#' Checks to ensure that long term data item contribution is consistent. This is
#' because often back end cahnges occur in hospitals that silently disrupt the
#' ETL process, and as such, some dataitems disappear without warning.
#'
#' @param x an extracted dataitem
#'
#' @return a table describing the coverage over a defined time window
#' @export
coverage <- function(x, ...) {
  UseMethod("coverage", x)
}

#' @export
coverage.default <- function(x, ...) {
  rlang::abort("There are no methods defined for this class")
}

#' @export
coverage.integer_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.string_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.real_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.date_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.time_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.datetime_1d <- function(x, reference_tbl) {
  x <- coverage_generic_1d(x, reference_tbl = reference_tbl)
}

#' @export
coverage_generic_1d <- function(x, reference_tbl = NULL) {
  name_check <- dplyr::intersect(
    names(x),
    c("out_of_bounds", "range_error", "duplicate")
  )
  if (length(name_check) != 3) {
    rlang::abort("You must supply a dataframe to `x` that contains columns with
                 names `out_of_bounds`, `range_error` and `duplicate`")
  }

  base_events <- x %>%
    filter(
      .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
      .data$range_error == 0L | is.na(.data$range_error),
      .data$duplicate == 0L | is.na(.data$duplicate)
    ) %>%
    left_join(
      reference_tbl %>% select(-site),
      by = "episode_id") %>%
    mutate(date = lubridate::as_date(start_date)) %>%
    group_by(site, date) %>%
    summarise(event_count = n_distinct(event_id))

  base_calendar <- reference_tbl %>%
    group_by(site) %>%
    summarise(
      start = lubridate::as_date(
        lubridate::floor_date(min(start_date), unit = "month")),
      end = lubridate::as_date(
        lubridate::ceiling_date(max(start_date), unit = "month")-1)) %>%
    tidyr::nest(start, end, .key = "date") %>%
    mutate(
      date = purrr::map(date, ~ seq.Date(.x$start, .x$end, by = "day"))) %>%
    unnest(date)

  out <- left_join(base_calendar, base_events, by = c("site", "date")) %>%
    filter(is.na(event_count)) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(site, year, month) %>%
    tally() %>%
    filter(n > 10) %>%
    arrange(.data$site, .data$year, .data$month)

  return(out)
}

#' @export
coverage.integer_2d <- function(x, reference_tbl) {
  x <- coverage_generic_2d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.string_2d <- function(x, reference_tbl) {
  x <- coverage_generic_2d(x, reference_tbl = reference_tbl)
}

#' @export
coverage.real_2d <- function(x, reference_tbl) {
  x <- coverage_generic_2d(x, reference_tbl = reference_tbl)
}

#' @export
coverage_generic_2d <- function(x, reference_tbl = NULL) {
  name_check <- dplyr::intersect(
    names(x),
    c("out_of_bounds", "range_error", "duplicate")
  )
  if (length(name_check) != 3) {
    rlang::abort("You must supply a dataframe to `x` that contains columns with
                 names `out_of_bounds`, `range_error` and `duplicate`")
  }

  base_events <- x %>%
    filter(
      .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
      .data$range_error == 0L | is.na(.data$range_error),
      .data$duplicate == 0L | is.na(.data$duplicate)
    ) %>%
    mutate(date = lubridate::as_date(datetime)) %>%
    group_by(site, date) %>%
    summarise(event_count = n_distinct(event_id))

  base_calendar <- reference_tbl %>%
    group_by(site) %>%
    summarise(
      start = lubridate::as_date(
        lubridate::floor_date(min(start_date), unit = "month")),
      end = lubridate::as_date(
        lubridate::ceiling_date(max(start_date), unit = "month")-1)) %>%
    tidyr::nest(start, end, .key = "date") %>%
    mutate(
      date = purrr::map(date, ~ seq.Date(.x$start, .x$end, by = "day"))) %>%
    unnest(date)

  out <- left_join(base_calendar, base_events, by = c("site", "date")) %>%
    filter(is.na(event_count)) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(site, year, month) %>%
    tally() %>%
    filter(n > 10) %>%
    arrange(.data$site, .data$year, .data$month)

  return(out)
}


#' verify Completeness
#'
#' @param x an extracted dataitem that has passed through verification
#'
#' @return a tibble with summary information on dataitem completeness
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter group_by summarise left_join n
verify_complete <- function(x, reference_tbl) {

  reference <- reference_tbl %>%
    group_by(.data$site) %>%
    summarise(total = n())

  x <- x %>%
    filter(
      .data$range_error == 0 | is.na(.data$range_error),
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate)
    ) %>%
    group_by(.data$site) %>%
    summarise(count = n()) %>%
    left_join(reference, by = "site") %>%
    mutate(completeness = count / total)
}
