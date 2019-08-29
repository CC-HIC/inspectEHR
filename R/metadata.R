#' Determine dimentionality of Events
#'
#' This function take a metadata table and produces a list of NIHR codes
#' describing whether the data is 1d (i.e. only measured once) or 2d (i.e "time
#' varying")
#'
#' @param metadata a collected metadata table
#'
#' @return a metadata table
#' @export
#'
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' make_dict(dplyr::collect(dplyr::tbl(ctn, "variables")))
#' DBI::dbDisconnect(ctn)
make_dict <- function(metadata = NULL) {
  if (is.null(metadata)) {
    stop("You need to provide a metadata (variables) table")
  }

  data_columns <- metadata %>%
    dplyr::select(-code_name, -long_name, -primary_column) %>%
    colnames()

  df <- metadata %>%
    collect()

  statics <- df %>%
    dplyr::mutate(nas = df %>%
      dplyr::select(data_columns) %>%
      base::apply(1, function(x) sum(!is.na(x)))) %>%
    dplyr::filter(nas == 1) %>%
    dplyr::select(code_name:primary_column) %>%
    dplyr::mutate(type = "1d")

  dynamics <- df %>%
    dplyr::filter(!(code_name %in% statics$code_name)) %>%
    dplyr::select(code_name:primary_column) %>%
    dplyr::mutate(type = "2d")

  dict <- rbind(statics, dynamics)
}
