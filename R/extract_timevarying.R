#' Extract & Reshape Timevarying Dateitems
#'
#' This is the workhorse function that transcribes 2d data from CC-HIC to a
#' table with 1 column per dataitem (and any metadata if relevent) and 1 row per
#' time per patient.
#'
#' The time unit is user definable, and set by the "cadance" argument. The
#' default behaviour is to produce a table with 1 row per hour per patient. If
#' there are duplicates/conflicts for example if there are more than 1 event for
#' a given hour, then duplicate rows are created This behaviour is intentional,
#' so that end researchers do not accidentally discard data that is contributed
#' on a high cadance.
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
#'   default) then all episodes are extracted. If working with the public
#'   dataset where episode ids are given as a character string of hashed values
#'   please use NULL.
#' @param code_names a vector of CC-HIC codes names to be extracted
#' @param rename a character vector of names you want to relabel CC-HIC codes
#'   as, or NULL (the default) if you do not want to relabel.
#' @param chunk_size a chunking parameter to help speed up the function and
#'   manage memory constaints
#' @param cadance a numerical scalar >= 0 or "timestamp". If a numerical scalar
#'   is used, it will describe the base time unit to build each row, in
#'   divisions of an hour. For example: 1 = 1 hour, 0.5 = 30 mins, 2 = 2 hourly.
#'   If multiple events occur within the specified time, then duplicate rows are
#'   created. If cadance = 0, then the pricise datetime will be used to generate
#'   the time column. This is likely to generate a large table, so use
#'   cautiously.
#'
#' @return sparse tibble with hourly cadance as rows, and unique hic events as
#'   columns
#' @export
#'
#' @importFrom purrr map imap
#' @importFrom lubridate now
#' @importFrom praise praise
#' @importFrom rlang inform
#' @importFrom dplyr distinct_at
#'
#' @examples
#' # DB Connection
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#'
#' # Extract Heart Rates for 5 episodes with default settings
#' hr_default <- extract_timevarying(ctn, episode_ids = 13639:13643, code_names = "NIHR_HIC_ICU_0108")
#' head(hr_default)
#' # Extract Heart Rates for 5 episodes with custom settings
#' hr_custom <- extract_timevarying(ctn, episode_ids = 13639:13643, code_names = "NIHR_HIC_ICU_0108", cadance = 2, overlap_method = mean)
#' head(hr_custom)
#' DBI::dbDisconnect(ctn)
extract_timevarying <- function(connection,
                                episode_ids = NULL,
                                code_names,
                                rename = NULL,
                                coalesce_rows = NULL,
                                chunk_size = 5000,
                                cadance = 1,
                                time_boundaries = c(-Inf, Inf)) {
  starting <- lubridate::now()

  if (!is.null(episode_ids) && class(episode_ids) != "integer") {
    rlang::abort("`episode_ids` must be given as NULL (the default) or an integer vector of episode ids")
  }

  cadance_pos_num <- class(cadance) == "numeric" && cadance >= 0
  cadance_timestamp <- cadance == "timestamp"

  if (!(cadance_pos_num || cadance_timestamp)) {
    rlang::abort("`cadance` must be given as a numeric scalar >= 0 or the string 'timestamp'")
  }

  if (!(any(code_names %in% "NIHR_HIC_ICU_0411"))) {
    exons <- append(code_names, "NIHR_HIC_ICU_0411")
  } else {
    exons <- code_names
  }

  if (any(code_names %in% "NIHR_HIC_ICU_0187")) {
    rlang::abort("NIHR_HIC_ICU_0187: Organism is not currently supported")
  }

  params <- tibble(
    code_names = code_names,
    short_names = rename,
    func = coalesce_rows
  )

  episode_groups <- dplyr::tbl(connection, "events") %>%
    select(episode_id) %>%
    distinct() %>%
    collect()

  if (!is.null(episode_ids)) {
    episode_groups <- filter(episode_groups, episode_id %in% episode_ids)
  }

  mdata <- collect(dplyr::tbl(connection, "variables"))

  episode_groups <- episode_groups %>%
    mutate(group = as.integer(seq(n()) / chunk_size)) %>%
    split(., .$group) %>%
    map(function(epi_ids) {
      collect_events <- dplyr::tbl(connection, "events") %>%
        filter(code_name %in% exons,
               episode_id %in% !!epi_ids$episode_id) %>%
        collect()

      map(collect_events %>%
        select(episode_id) %>%
        distinct() %>%
        pull(), process_all,
      events = collect_events,
      metadata = mdata,
      cadance = cadance,
      coalesce_rows = params,
      time_boundaries = time_boundaries
      ) %>%
        bind_rows()
    }) %>%
    bind_rows()

  if (!is.null(rename)) {
    for (i in seq_len(nrow(params))) {
      names(episode_groups) <- gsub(
        pattern = params$code_names[i],
        replacement = params$short_names[i],
        x = names(episode_groups)
      )
    }
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
    well_done <-
      praise::praise(
        "${EXCLAMATION}! How ${adjective} was that?!"
        )
    rlang::inform(well_done)
  }

  return(episode_groups)
}


process_all <- function(epi_id, events, metadata, cadance, coalesce_rows, time_boundaries) {
  pt <- events %>%
    filter(episode_id == epi_id) %>%
    mutate(datetime = as.POSIXct(datetime))

  start_time <- pt %>%
    filter(code_name == "NIHR_HIC_ICU_0411") %>%
    select(datetime) %>%
    pull()

  if (!identical(time_boundaries, c(-Inf, Inf))) {
    pull_from <- start_time + lubridate::hours(time_boundaries[1])
    pull_to <- start_time + lubridate::hours(time_boundaries[2])
    pt <- pt %>%
      filter(datetime >= pull_from,
             datetime <= pull_to)
  }

  if (class(cadance) == "numeric") {
    imap(
      pt %>%
        filter(code_name %in% find_2d(metadata)$code_name) %>%
        arrange(code_name) %>%
        split(., .$code_name),
      process_item,
      metadata = metadata,
      start_time = start_time,
      cadance = cadance,
      coalesce_rows = coalesce_rows
      ) %>%
        reduce(
          full_join, by = "diff_time",
          .init = tibble(diff_time = as.numeric(NULL))) %>%
        rename(time = diff_time) %>%
        mutate(episode_id = epi_id) %>%
        arrange(time)
  } else {
    imap(
      pt %>%
        filter(code_name %in% find_2d(metadata)$code_name) %>%
        arrange(code_name) %>%
        split(., .$code_name),
      process_item_timestamp,
      metadata = metadata,
      coalesce_rows = coalesce_rows
      ) %>%
      reduce(full_join, by = "time_stamp",
             .init = tibble(time_stamp = lubridate::ymd_hms(NULL))) %>%
      rename(time = time_stamp) %>%
      mutate(episode_id = epi_id) %>%
      arrange(time)
  }
}


process_item <- function(df, var_name, metadata, start_time, cadance, coalesce_rows) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  summary_func <- coalesce_rows %>%
    filter(code_names == var_name) %>%
    select(func) %>%
    pull() %>%
    `[[`(1)

  tb_a <- df %>%
    mutate(
      diff_time = as.numeric(difftime(datetime, start_time, units = "hours")))

  if (cadance > 0) {
    tb_a <- tb_a %>%
      mutate(diff_time = round_any(diff_time, cadance))
  }

  if (length(meta_names) == 0) {
    tb_a <- tb_a %>%
      distinct(diff_time, .keep_all = TRUE) %>%
      rename(!!var_name := prim_col) %>%
      select(diff_time, !!var_name)
  } else {
    tb_a <- tb_a %>%
      distinct_at(vars(diff_time, prim_col, meta_names), .keep_all = TRUE) %>%
      rename(!!var_name := prim_col) %>%
      select(diff_time, !!var_name, !!!meta_names) %>%
      mutate_at(vars(meta_names), function(x) {
        x <- as.character(x)
        if_else(is.na(x), "0", x)
      }) %>%
      split(.[meta_names[1]]) %>%
      map(function(x) {
        append_n <- distinct_at(x, vars(meta_names)) %>%
          pull() %>%
          as.character()

        new_name <- paste(var_name, append_n, sep = "_")
        names(x) <- if_else(names(x) == var_name, new_name, names(x))

        select(x, -c(!!!meta_names)) %>%
          group_by(diff_time) %>%
          summarise_at(vars(new_name), summary_func, na.rm = TRUE)
      }) %>%
      reduce(full_join, by = "diff_time",
             .init = tibble(diff_time = as.numeric(NULL)))
  }
  return(tb_a)
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
process_item_timestamp <- function(df, var_name, metadata) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  tb_a <- df %>%
    rename(time_stamp = datetime) %>%
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
        episode_id = as.numeric(epi_id),
        time = seq(
          min(base_table$time, 0),
          max(base_table$time, 0),
          by = cadance
        )
      )
    }) %>%
    bind_rows() %>%
    left_join(df, by = c("episode_id", "time"))
}
