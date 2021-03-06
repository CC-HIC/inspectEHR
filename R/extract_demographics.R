#' Extract 1d data from CC-HIC
#'
#' Takes a remote database contection to CC-HIC, a vector of HIC codes (and
#' optionally a vector of labels to rename the codes) and returns a table with 1
#' row per patient and 1 column per data item.
#'
#' @param connection a CC-HIC database connection
#' @param episode_ids an integer vector of episode ids from the CC-HIC DB
#'   that you want to extact. The default (NULL) is to extract all.
#' @param code_names a character vector of CC-HIC codes
#' @param rename a character vector of names you want to relabel CC-HIC codes
#'   as, or NULL (the default) if you do not want to relabel.
#'
#' @export
#'
#' @importFrom dplyr collect select mutate filter inner_join full_join if_else
#'   summarise_all select_if
#' @importFrom tidyr spread
#' @importFrom tibble as_tibble
#' @importFrom purrr reduce
#' @importFrom rlang !!! inform abort warn .data
#'
#' @return A tibble of 1d data
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' hic_codes <- "NIHR_HIC_ICU_0409"
#' new_labels <- "apache_score"
#' extract_demographics(ctn, episode_ids = 13639:13643, hic_codes, new_labels)
#' DBI::dbDisconnect(ctn)
extract_demographics <- function(connection = NULL, episode_ids = NULL,
                                 code_names = NULL, rename = NULL) {
  stopifnot(!any(is.null(connection), is.null(code_names)))

  if (!is.null(episode_ids) && class(episode_ids) != "integer") {
    rlang::abort(
      "`episode_ids` must be given as NULL (the default) or an
      integer vector of episode ids")
  }

  tbls <- retrieve_tables(connection)

  demographics <- tbls[["variables"]] %>%
    collect() %>%
    mutate(nas = tbls[["variables"]] %>%
      select(-.data$code_name, -.data$long_name, -.data$primary_column) %>%
      collect() %>%
      tibble::as_tibble() %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    filter(.data$nas == 1) %>%
    select(.data$code_name, .data$primary_column)

  all_demographic_codes <- demographics$code_name
  extract_codes <- all_demographic_codes[all_demographic_codes %in% code_names]

  if (length(extract_codes) != length(code_names)) {
    rlang::warn(
      "You are trying to extract non-1d data.
      Consider using `extract_timevarying()`")
  }

  if (is.null(episode_ids)) {
    tb_base <- tbls[["events"]] %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% extract_codes) %>%
      collect()
  } else {
    tb_base <- tbls[["events"]] %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% extract_codes,
             .data$episode_id %in% episode_ids) %>%
      collect()
  }

  complete_fields <- tb_base %>%
    select(-.data$episode_id, -.data$code_name) %>%
    dplyr::select_if(function(x) !(all(is.na(x)))) %>%
    names()

  tb_segments <- vector(mode = "list", length = length(complete_fields))

  for (i in seq_along(complete_fields)) {
    tb_segments[[i]] <- extract_demographics_helper(
      tb_base, complete_fields[i], demographics)
  }

  db_1 <- purrr::reduce(
    tb_segments,
    full_join,
    by = "episode_id"
  )

  if (!is.null(rename)) {
    replacement_names <- rename[match(names(db_1), code_names)]
    names(db_1) <- if_else(
      is.na(replacement_names), names(db_1), replacement_names)
  }

  if (is.null(rename)) {
    lookups <- tibble(codes = code_names,
                      names = code_names)
  } else {
    lookups <- tibble(codes = code_names,
                      names = rename)
  }

  attr(db_1, "lookups") <- lookups
  class(db_1) <- append(class(db_1), "1-dim", after = 0)

  return(db_1)
}


#' @importFrom rlang .data !! enquo
#' @importFrom dplyr select inner_join filter
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
extract_demographics_helper <- function(tb_base, col_name, demographics) {
  quo_column <- enquo(col_name)

  tb_section <- tb_base %>%
    select(.data$code_name, !!quo_column, .data$episode_id) %>%
    inner_join(
      demographics %>%
        filter(.data$primary_column == !!quo_column),
      by = "code_name"
    ) %>%
    select(-.data$primary_column) %>%
    spread(key = code_name, value = !!quo_column)

  return(tb_section)
}
