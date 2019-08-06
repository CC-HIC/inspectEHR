#' Establishes a connection to the database
#'
#' @param host db host: typically "localhost"
#' @param username db username
#' @param password db password
#' @param database target db
#' @param system choice of either "postgres" or "sqlite" depending upon your backend
#' @param file filename as character vector if using an sqlite db
#'
#' @return a database object connection
#' @export
#'
#' @examples
#' ctn <- connect(sqlite_file = "path/to/file.sqlite3")
#' ctn <- connect(database = "hic", username = "abdce", password = "qwerty")
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
      dbname = database)

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
#' @importFrom dplyr db_list_tables
#'
#' @return a list containing pointers to tables within the sql connection.
#' @export
#'
#' @examples
#' \dontrun{
#' tbls <- retrieve_tables(ctn)
#' tbls[["events"]] # the events table
#' }
retrieve_tables <- function(connection) {

  if(missing(connection)) {
    stop("a connection must be provided")
  }

  all_tables <- dplyr::db_list_tables(connection)
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
#' make_reference(ctn)
make_reference <- function(connection) {

    episodes <- dplyr::tbl(connection, "episodes")
    provenance <- dplyr::tbl(connection, "provenance")

    reference <- left_join(episodes, provenance, by = c("provenance" = "file_id")) %>%
      select(episode_id, nhs_number, start_date, site)

    # Accounts for lack of datetime type in SQLite
    if (class(connection)[1] == "SQLiteConnection") {
      reference <- reference %>%
        collect() %>%
        mutate(start_date = lubridate::ymd_hms(start_date))

      return(reference)

    } else {

      reference <- collect(reference)
      return(reference)

    }
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
#' make_core(ctn)
make_core <- function(connection) {

  events <- dplyr::tbl(connection, "events")
  episodes <- dplyr::tbl(connection, "episodes")
  provenance <- dplyr::tbl(connection, "provenance")

  core <- episodes %>%
    left_join(provenance, by = c("provenance" = "file_id")) %>%
    inner_join(events, by = "episode_id")

  return(core)

}

find_max_time <- function(events, time_col) {

  quo_timecol <- enquo(time_col)

  max_time <- events %>%
    group_by(episode_id) %>%
    summarise(maxtime = max(!! quo_timecol, na.rm = TRUE)) %>%
    collect() %>%
    mutate(maxtime = as.POSIXct(maxtime, origin = "1970-01-01 00:00:00"))

  return(max_time)

}

#' Custom Error Capturing
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
round_any <- function(x, accuracy = 1){
  round(x/accuracy)*accuracy
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
