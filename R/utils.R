#' Establishes a connection to the database
#'
#' @param database target db
#' @param host db host: typically "localhost"
#' @param port db port number
#' @param username db username
#' @param password db password
#' @param sqlite_file filename as character vector if using an sqlite db (in
#'   which case this is the only argument that needs to be supplied)
#'
#' @return a database object connection
#' @export
#'
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' class(ctn)
#' DBI::dbDisconnect(ctn)
connect <- function(database = NULL,
                    host = "localhost",
                    port = 5432,
                    username = NULL,
                    password = NULL,
                    sqlite_file = NULL) {
  stopifnot(
    all(!is.null(database), !is.null(username), !is.null(password)) ||
      !is.null(sqlite_file)
  )

  if (is.null(sqlite_file)) {
    connection <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = host,
      port = port,
      user = username,
      password = password,
      dbname = database
    )
  } else {
    connection <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  }

  return(connection)
}


#' Retrieve Database Tables
#'
#' Places all tables from the database connection in a list. This makes use of the tables
#' in dplyr extremely easy.
#'
#' @param connection an sql connection
#'
#' @importFrom DBI dbListTables
#' @importFrom dplyr tbl
#'
#' @return a list containing pointers to tables within the sql connection.
#' @export
#'
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' core <- make_core(ctn)
#' tbls <- retrieve_tables(ctn)
#' tbls[["events"]] # the events table
#' DBI::dbDisconnect(ctn)
retrieve_tables <- function(connection) {
  if (missing(connection)) {
    stop("a connection must be provided")
  }

  all_tables <- DBI::dbListTables(connection)
  tbl_list <- list()

  for (i in seq_along(all_tables)) {
    tbl_list[[all_tables[i]]] <- dplyr::tbl(connection, all_tables[i])
  }

  return(tbl_list)
}


#' Make reference Table
#'
#' Makes the reference table used my many functions in this package.
#' This is part of the basic setup.
#'
#' @param connection a database connection
#'
#' @return a tibble with episode level data with site
#' @export
#'
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' ref <- make_reference(ctn)
#' head(ref)
#' DBI::dbDisconnect(ctn)
make_reference <- function(connection) {
  episodes <- dplyr::tbl(connection, "episodes")
  provenance <- dplyr::tbl(connection, "provenance")

  reference <- left_join(
    episodes, provenance, by = c("provenance" = "file_id")) %>%
    select(episode_id, nhs_number, start_date, site)

  # Accounts for lack of datetime type in SQLite
  if (class(connection)[1] == "SQLiteConnection") {
    reference <- reference %>%
      collect() %>%
      mutate(start_date = lubridate::ymd_hms(start_date))
  } else {
    reference <- collect(reference)
  }

  class(reference) <- append(class(reference), "tbl_ref", after = 0)
  return(reference)
}


#' Core table
#'
#' Writes a remote SQL query to make a core table with all the necessary column
#' names for most data extraction tasks. No work is done till the function is
#' explicitly called.
#'
#' @param connection a database connection
#'
#' @return a database object
#' @export
#'
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' core <- make_core(ctn)
#' head(core)
#' DBI::dbDisconnect(ctn)
make_core <- function(connection) {
  events <- dplyr::tbl(connection, "events")
  episodes <- dplyr::tbl(connection, "episodes")
  provenance <- dplyr::tbl(connection, "provenance")

  core <- episodes %>%
    left_join(provenance, by = c("provenance" = "file_id")) %>%
    inner_join(events, by = "episode_id")
}

find_max_time <- function(events, time_col) {
  quo_timecol <- enquo(time_col)

  max_time <- events %>%
    group_by(episode_id) %>%
    summarise(maxtime = max(!!quo_timecol, na.rm = TRUE)) %>%
    collect() %>%
    mutate(maxtime = as.POSIXct(maxtime, origin = "1970-01-01 00:00:00"))

  return(max_time)
}

is.error <- function(x) {
  inherits(x, "try-error")
}


#' Round any
#'
#' rounds a numeric value to any arbitrary degree of precision.
#' defaults to nearest whole integer
#'
#' @param x a numeric vector
#' @param accuracy a numeric value specifying the base for rounding
#'
#' @return a vector of the same length as \code{x} rounded to the defined accuracy
#' @export
#'
#' @examples
#' round_any(c(1, 1.25, 1.5, 1.75, 2), accuracy = 0.5)
round_any <- function(x, accuracy = 1) {
  round(x / accuracy) * accuracy
}


#' Round to percentage
#'
#' @param x numerator
#' @param n denominator
#' @param digits rounding
#'
#' @return a vector of the same length as \code{x} rounded to the defined digit
#'   length given as a percentage for display
#' @export
#'
#' @examples
round_perc <- function(x, n, digits = 0) {
  round((x/n)*100, digits = digits)
}


check_columns <- function(x, name_x) {
  row_count <- collect(tally(x))$n

  x %>%
    summarise_all(~ sum(is.na(.), na.rm = TRUE)) %>%
    collect() %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing") %>%
    add_column(table = name_x, .before = TRUE) %>%
    mutate(percentage = round_perc(missing, row_count))
}



#' Inverse Logistic Function
#'
#' @param x a numeric vector on the logistic scale
#'
#' @return a numeric vector on the probability scale
#' @export
#'
#' @examples
#' inv_logit(-4:4)
inv_logit <- function(x) {
  p <- 1 / (1 + exp(-x))
  ifelse(x == Inf, 1, p)
}


#' Daily Events for HIC Data item by Site
#'
#' Calculates the number of events contributed for each calendar day, stratified
#' by site. This is a complete table, i.e. days with 0 admissions are not
#' listed
#'
#' @param df extracted data item
#' @param reference the reference table generated by \code{\link{make_reference}}
#' @param by_site the named site of interest as a character
#'
#' @return a tibble with the number of unique episodes admitted for a given day
#'
#' @importFrom dplyr filter mutate group_by summarise n_distinct
#' @importFrom lubridate date
daily_events <- function(df = NULL, reference = NULL, by_site = NULL) {

  admissions <- df %>%
    filter(.data$site == by_site) %>%
    left_join(reference %>%
                select(episode_id, start_date), by = "episode_id") %>%
    mutate(date = lubridate::as_date(start_date)) %>%
    group_by(date) %>%
    summarise(events = n_distinct(event_id)) %>%
    filter(events > 0)
}

# ===== CLASS CHECKING

is.string_2d <- function(x) inherits(x, "string_2d")
is.string_1d <- function(x) inherits(x, "string_1d")
is.integer_2d <- function(x) inherits(x, "integer_2d")
is.integer_1d <- function(x) inherits(x, "integer_1d")
is.real_2d <- function(x) inherits(x, "real_2d")
is.real_1d <- function(x) inherits(x, "real_1d")
is.datetime_1d <- function(x) inherits(x, "datetime_1d")
is.date_1d <- function(x) inherits(x, "date_1d")
is.time_1d <- function(x) inherits(x, "time_1d")
