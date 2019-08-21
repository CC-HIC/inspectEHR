#' Adds episodes without input variables as NA
#'
#' Returns a list with 2 tibbles of the exact same form as produced by
#' \code{\link{process_dbl}} NAs can be found in position 2 ("empty"). This is
#' useful in understanding how many episodes do not report on a certain field.
#' As both lists are in the exact same form, then can be bound easily.
#'
#' @param processed_table the processed table from process_dbl
#' @param reference_table the reference table
#'
#' @return A list with the original processed data in position 1 and missing
#'   data in 2
add_na <- function(x, ...) {
  UseMethod("add_na", x)
}


#' @export
add_na.hic_dbl <- function(x = NULL, reference_table = NULL) {

  # Identify those who have some data for this chosen input variable
  havedata <- x[["episode_id"]] %>%
    base::unique()

  nodata <- reference_table %>%
    dplyr::select(site, episode_id) %>%
    dplyr::filter(!(episode_id %in% havedata))

  return(nodata)
}


#' @export
add_na.hic_int <- function(x = NULL, reference_table = NULL) {

  # Identify those who have some data for this chosen input variable
  havedata <- x[["episode_id"]] %>%
    base::unique()

  nodata <- reference_table %>%
    dplyr::select(site, episode_id) %>%
    dplyr::filter(!(episode_id %in% havedata))

  return(nodata)
}
