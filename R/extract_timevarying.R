#' Extract & Reshape Timevarying Dateitems
#'
#' This is the workhorse function that transcribes 2d data from CC-HIC to a
#' table with 1 column per dataitem (and any metadata if relevent) and 1 row per
#' time per patient.
#'
#' The time unit is user definable, and set by the "cadance" argument. The
#' default behaviour is to produce a table with 1 row per hour per patient.
#'
#' Many events inside CC-HIC occur on a greater than hourly basis. Depending
#' upon the chosen analysis, you may which to increase the cadance. 0.5 for
#' example will produce a table with 1 row per 30 minutes per patient.
#'
#' Where you are extacting at a resolution lower than is recorded in the
#' database, you can specify a summary function with the \code{overlap_method}
#' argument. This argument takes a summary function as an argument, for example,
#' mean and will apply this behaviour to all data items in the database. At
#' present, this doesn't deal with: functions that act differently across
#' different data types, or metadata.
#'
#' Choose what variables you want to pull out wisely. This function is actually
#' quite efficient considering what it needs to do, but it can take a very long
#' time if pulling out lots of data. It is a strong recomendation that you
#' optimise the database with indexes prior to using this function.
#'
#' It is perfectly possible for this function to produce negative time rows. If,
#' for example a patient had a measure taken in the hours before they were
#' admitted, then this would be added to the table with a negative time value.
#' As a concrete example, if a patient had a sodium measured at 08:00, and they
#' were admitted to the ICU at 20:00 the same day, then the sodium would be
#' displayed at time = -12. This is normal behaviour it is left to the end user
#' to determine how best they wish to account for this.
#'
#' @param connection a CC-HIC database connection
#' @param episode_ids an integer vector of episode_ids or NULL. If NULL (the
#'   default) then all episodes are extracted
#' @param code_names a vector of CC-HIC codes names to be extracted
#' @param chunk_size a chunking parameter to help speed up the function and
#'   manage memory constaints
#' @param cadance a numerical scalar or one of "exact" or "timestamp". If a
#'   numerical scalar is used, it will describe the base time unit to build each
#'   row, in divisions of an hour. For example: 1 = 1 hour, 0.5 = 30 mins, 2 = 2
#'   hourly. If multiple events occur within the specified time, then the first
#'   is chosen and the others are dropped. If cadance = "exact", then the EXACT
#'   datetime will be used at the time column. This is likely to generate a
#'   LARGE table, so use cautiously.
#' @param overlap_method a summary function to adress the issue of extracting
#'   data that is contributed at a higher resolution than your cadance. The
#'   default behaviour is "distinct" which simply disgards any excess data.
#'
#' @return sparse tibble with hourly cadance as rows, and unique hic events as
#'   columns
#' @export
#'
#' @importFrom purrr map imap
#' @importFrom lubridate now
#' @importFrom praise praise
#' @importFrom rlang inform
#'
#' @examples
#' # DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_path)
#'
#' # Extract Heart Rates for 5 episodes with default settings
#' hr_default <- extract_timevarying(ctn, episode_ids = 13639:13643, code_names = "NIHR_HIC_ICU_0108")
#' head(hr_default)
#' # Extract Heart Rates for 5 episodes with custom settings
#' hr_custom <- extract_timevarying(ctn, episode_ids = 13639:13643, code_names = "NIHR_HIC_ICU_0108", cadance = 2, overlap_method = mean)
#' head(hr_custom)
#' DBI::dbDisconnect(ctn)
extract_timevarying <- function(connection, episode_ids = NULL, code_names,
                                rename = NULL, chunk_size = 5000, cadance = 1,
                                overlap_method = "distinct") {
  starting <- lubridate::now()

  if (!is.null(episode_ids) && class(episode_ids) != "integer") {
    rlang::abort("`episode_ids` must be given as NULL (the default) or an
                 integer vector of episode ids")
  }

  if (!(any(code_names %in% "NIHR_HIC_ICU_0411"))) {
    exons <- append(code_names, "NIHR_HIC_ICU_0411")
  } else {
    exons <- code_names
  }

  if (class(overlap_method) != "function") {
    if (overlap_method != "distinct") {
      rlang::abort(
        "overlap method must equal `distinct` or be a summary function")
    }
  }

  if (class(overlap_method) == "function") {
  rlang::warn("metadata extraction is not yet supported with this feature")
  }

  episode_groups <- dplyr::tbl(connection, "events") %>%
    select(episode_id) %>%
    distinct() %>%
    collect()

  if (!is.null(episode_ids)) {
    episode_groups <- filter(episode_groups, episode_id %in% episode_ids)
  }

  episode_groups <- episode_groups %>%
    mutate(group = as.integer(seq(n()) / chunk_size)) %>%
    split(., .$group) %>%
    map(function(epi_ids) {
      collect_events <- dplyr::tbl(connection, "events") %>%
        filter(code_name %in% exons) %>%
        filter(episode_id %in% !!epi_ids$episode_id) %>%
        collect()

      map(collect_events %>%
        select(episode_id) %>%
        distinct() %>%
        pull(), process_all,
      events = collect_events,
      metadata = collect(dplyr::tbl(connection, "variables")),
      cadance = cadance,
      overlap_method = overlap_method
      ) %>%
        bind_rows()
    }) %>%
    bind_rows()

  if (!is.null(rename)) {
    replacement_names <- rename[match(names(episode_groups), code_names)]
    names(episode_groups) <- if_else(
      is.na(replacement_names), names(episode_groups), replacement_names)
  }

  if (is.null(rename)) {
    lookups <- tibble(codes = code_names,
                      names = code_names)
  } else {
    lookups <- tibble(codes = code_names,
                      names = rename)
  }

  attr(episode_groups, "lookups") <- lookups
  class(episode_groups) <- append(class(episode_groups), "2-dim", after = 0)

  elapsed_time <- signif(
    as.numeric(
      difftime(
        lubridate::now(), starting, units = "hour")), 2)
  inform(paste(elapsed_time, "hours to process"))

  if (requireNamespace("praise", quietly = TRUE)) {
    praise(
 "${Exclamation}! ${EXCLAMATION}!-${EXCLAMATION}! How ${adjective} was that?!")
    }

  return(episode_groups)
}


process_all <- function(epi_id, events, metadata, cadance, overlap_method) {
  pt <- events %>%
    filter(episode_id == epi_id)

  start_time <- pt %>%
    filter(code_name == "NIHR_HIC_ICU_0411") %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    select(datetime) %>%
    pull()

  if (cadance == "exact") {
    imap(pt %>%
      filter(code_name %in% find_2d(metadata)$code_name) %>%
      arrange(code_name) %>%
      split(., .$code_name), process_episode_exact,
    metadata = metadata,
    start_time = start_time
    ) %>%
      reduce(
        full_join, by = "r_diff_time",
        .init = tibble(r_diff_time = as.numeric(NULL))) %>%
      rename(time = r_diff_time) %>%
      mutate(episode_id = epi_id) %>%
      arrange(time)
  } else if (cadance == "timestamp") {
    imap(pt %>%
      filter(code_name %in% find_2d(metadata)$code_name) %>%
      arrange(code_name) %>%
      split(., .$code_name), process_episode_timestamp,
    metadata = metadata
    ) %>%
      reduce(full_join, by = "time_stamp",
             .init = tibble(time_stamp = lubridate::ymd_hms(NULL))) %>%
      rename(time = time_stamp) %>%
      mutate(episode_id = epi_id) %>%
      arrange(time)
  } else {
    imap(pt %>%
      filter(code_name %in% find_2d(metadata)$code_name) %>%
      arrange(code_name) %>%
      split(., .$code_name), process_episode,
    metadata = metadata,
    start_time = start_time,
    cadance = cadance,
    overlap_method = overlap_method
    ) %>%
      reduce(full_join, by = "r_diff_time",
             .init = tibble(r_diff_time = as.numeric(NULL))) %>%
      rename(time = r_diff_time) %>%
      mutate(episode_id = epi_id) %>%
      arrange(time)
  }
}


process_episode <- function(df, var_name, metadata, start_time, cadance,
                            overlap_method) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  qp <- enquo(prim_col)

  meta_names <- find_2d_meta(metadata, var_name)

  if (class(overlap_method) != "function") {
    tb_a <- df %>%
      mutate(datetime = as.POSIXct(datetime)) %>%
      mutate(diff_time = difftime(datetime, start_time, units = "hours")) %>%
      mutate(r_diff_time = as.numeric(round_any(diff_time, cadance))) %>%
      distinct(r_diff_time, .keep_all = TRUE) %>%
      select(-event_id, -episode_id, -datetime, -code_name, -diff_time) %>%
      rename(!!var_name := prim_col) %>%
      select(r_diff_time, !!var_name, !!!meta_names)
  } else {
    tb_a <- df %>%
      mutate(datetime = as.POSIXct(datetime)) %>%
      mutate(diff_time = difftime(datetime, start_time, units = "hours")) %>%
      mutate(r_diff_time = as.numeric(round_any(diff_time, cadance))) %>%
      group_by(r_diff_time) %>%
      summarise(
        overlap_col = overlap_method(.data[[prim_col]], na.rm = TRUE)) %>%
      rename(!!var_name := overlap_col)
  }
  if (length(meta_names) == 0) {
    return(tb_a)
  }

  names(meta_names) <- paste(var_name, "meta", seq_along(meta_names), sep = ".")
  rename(tb_a, !!!meta_names)
}


#' Process episode exactly
#'
#' Process a single episode from the CC-HIC databse into a rectangular table
#' with a time column corresponding to the exact period (in hours) from
#' admission.
#'
#' @param df a dataframe containing all episode information (It is unlikely that
#'   this will be accessed directly)
#' @param var_name the CC-HIC codename for the current variable being processed
#' @param metadata the CC-HIC metadata table
#' @param start_time the episode start time (or whatever other anchor you wish
#'   to use)
process_episode_exact <- function(df, var_name, metadata, start_time) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  tb_a <- df %>%
    mutate(datetime = as.POSIXct(datetime, origin = "1970-01-01 00:00:00")) %>%
    mutate(
      r_diff_time = as.numeric(
        difftime(
          datetime, start_time, units = "hours"))) %>%
    distinct(r_diff_time, .keep_all = TRUE) %>%
    select(-event_id, -episode_id, -datetime, -code_name) %>%
    rename(!!var_name := prim_col) %>%
    select(r_diff_time, !!var_name, !!!meta_names)

  if (length(meta_names) == 0) {
    return(tb_a)
  }

  names(meta_names) <- paste(var_name, "meta", seq_along(meta_names), sep = ".")
  rename(tb_a, !!!meta_names)
}


#' Process episode with a timestamp
#'
#' Process a single episode into a rectangular table with a timestamp column
#' instead of the usual difftime since admission. The timestamp column
#' corresponds to the exact timestamp of the event of interest. This is
#' particularly useful for when you need to combine episodes into spells. Take
#' care around times of clock change.
#'
#' @param df a dataframe containing all episode information (It is unlikely that
#'   this will be accessed directly)
#' @param var_name the CC-HIC codename for the current variable being processed
#' @param metadata the CC-HIC metadata table
process_episode_timestamp <- function(df, var_name, metadata) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  tb_a <- df %>%
    mutate(
      time_stamp = as.POSIXct(
        datetime, origin = "1970-01-01 00:00:00")) %>%
    select(-event_id, -episode_id, -code_name, -datetime) %>%
    rename(!!var_name := prim_col) %>%
    select(time_stamp, !!var_name, !!!meta_names)

  if (length(meta_names) == 0) {
    return(tb_a)
  }

  names(meta_names) <- paste(var_name, "meta", seq_along(meta_names), sep = ".")
  rename(tb_a, !!!meta_names)
}



not_na <- function(x) {
  any(!is.na(x))
}


find_2d <- function(metadata) {
  metadata %>%
    dplyr::mutate(nas = metadata %>%
      dplyr::select(-code_name, -long_name, -primary_column) %>%
      collect() %>%
      tibble::as.tibble() %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    dplyr::filter(nas > 1) %>%
    dplyr::select(code_name, primary_column)
}


find_2d_meta <- function(metadata, c_name) {
  select_row <- metadata %>%
    filter(code_name == c_name)

  prim_col <- select_row %>%
    select(primary_column) %>%
    pull()

  select_row %>%
    select(-code_name, -long_name, -primary_column, -datetime, -!!prim_col) %>%
    select_if(.predicate = not_na) %>%
    names()
}



#' Fill in 2d Table to make a Sparse Table
#'
#' The extract_timevarying returns a non-sparse table (i.e. rows/hours with
#' no recorded information for a patient are not presented in the table)
#' This function serves to expand the table and fill missing rows with NAs.
#' This is useful when working with most time-series aware stats packages
#' that expect a regular cadance to the table.
#'
#' @param df a dense time series table produced from extract_timevarying
#' @param cadance the cadance by which you want to expand the table
#'   (default = 1 hour)
#'
#' @return a sparse time series table
#' @export
expand_missing <- function(df, cadance = 1) {
  df %>%
    select(episode_id, time) %>%
    split(., .$episode_id) %>%
    imap(function(base_table, epi_id) {
      tibble(
        episode_id = epi_id,
        time = seq(min(base_table$time, 0),
          max(base_table$time, 0),
          by = cadance
        )
      )
    }) %>%
    bind_rows() %>%
    left_join(df, by = c("episode_id", "time"))
}
