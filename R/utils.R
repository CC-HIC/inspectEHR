#' Retrieve Database Tables
#'
#' Places all tables from the database connection in a list. This makes use of the tables
#' in dplyr extremely easy.
#'
#' @param connection an sql connection
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema
#' @importFrom purrr map
#'
#' @return a list containing pointers to tables within the sql connection.
#' @export
retrieve_tables <- function(connection, schema) {
  
  if (missing(connection)) {
    stop("a connection must be provided")
  }
  
  schema_tbls <- dbGetQuery(
    ctn, paste0(
      "SELECT * FROM information_schema.tables WHERE table_schema = '",
      schema,
      "'")
      )

  tbls <- purrr::map(
    schema_tbls$table_name, ~ tbl(ctn, in_schema("ops_dev", .x)))
  
  names(tbls) <- schema_tbls$table_name

  return(tbls)
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
