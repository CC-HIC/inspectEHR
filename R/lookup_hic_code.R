#' Lookup CC-HIC Code
#'
#' Perform a quick search for a CC-HIC Code
#'
#' @param search search term as either a code (numeric) or name (character)
#'
#' @return a table with query results
#' @export
#'
#' @examples
#' lookup_hic()
lookup_hic <- function(search = 0108) {
  if (is.numeric(search)) {
    search_result <- qref %>%
      filter(grepl(search, code_name)) %>%
      select(code_name, short_name)
  } else if (is.character(search)) {
    search_result <- qref %>%
      filter(grepl(search, short_name, ignore.case = TRUE)) %>%
      select(code_name, short_name)
  }

  if (nrow(search_result) == 0) {
    print("no search results")
  } else {
    print(search_result)
  }
}
