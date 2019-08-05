#' Validate Episodes Based Upon Patchy Data
#'
#' Determines which months are invalid based on a low contribution of data
#' This is based upon the long term daily average for admissions. Days that
#' fall below 2 SD of the long term mean are tagged. If more than the
#' threshold value occur in a single month, the month is removed from
#' further analysis
#'
#' This function removes episodes that occur during particularly sparse periods
#' (as this is likely that these months are contributing poor data) and return
#' only episodes that have a logical consistency in non-sparse months
#'
#' @param episodes collected episodes table
#' @param provenance collected provenance table
#' @param threshold threshold number of days to use in calculation
#'
#' @return a tibble of months that are to be excluded from future analysis
#' @export
#'
#' @importFrom dplyr mutate group_by summarise left_join right_join n_distinct
#' distinct tibble bind_rows pull
#' @importFrom lubridate date year month wday
#'
#' @examples
#' invalid_months(episodes, provenance)
#' invalid_months(episodes, provenance, theshold = 15)
validate_episodes <- function(
  episode_length_table,
  reference_table,
  all_sites,
  threshold = 10) {

  # The typical admissions for a given day of the week within a year window
  # This helps to account for seasonality and trend changes over time
  typical_admissions <- reference_table %>%
    dplyr::mutate(date = lubridate::date(start_date)) %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarise(episode_count = n_distinct(episode_id)) %>%
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date, label = TRUE),
                  wday = lubridate::wday(date, label = TRUE)) %>%
    dplyr::group_by(site, year, wday) %>%
    dplyr::summarise(mean_episodes = mean(episode_count),
                     sd_episode = sd(episode_count))

  # too_few tells me the days when admissions fell under the expected
  too_few <- reference_table %>%
    dplyr::mutate(date = lubridate::date(start_date)) %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarise(episodes = n()) %>%
    dplyr::mutate(year = lubridate::year(date),
                  wday = lubridate::wday(date, label = TRUE)) %>%
    dplyr::left_join(typical_admissions, by = c("site" = "site",
                                                "year" = "year",
                                                "wday" = "wday")) %>%
    dplyr::mutate(
      too_few = ifelse(
        episodes < (mean_episodes - 2*sd_episode), TRUE, FALSE)) %>%
    dplyr::filter(too_few == TRUE) %>%
    dplyr::select(site, date)

  # what we don't capture properly is days when there is no data - i.e. NAs
  # this is what we will fix here
  na_days <- reference_table %>%
    dplyr::mutate(date = lubridate::date(start_date)) %>%
    dplyr::select(site, date) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    dplyr::mutate(admission = TRUE)

  ds <- tibble(
    date = rep(
      seq.Date(
        from = min(lubridate::date(reference_table$start_date)),
        to = max(lubridate::date(reference_table$start_date)),
        by = "day"),
      times =  length(all_sites)))

  ds <- ds %>%
    mutate(site = rep(all_sites, each = nrow(ds)/length(all_sites)))

  too_few_all <- na_days %>%
    dplyr::right_join(ds,
                      by = c("date" = "date",
                             "site" = "site")) %>%
    dplyr::filter(is.na(admission)) %>%
    dplyr::select(-admission) %>%
    dplyr::bind_rows(too_few)

  ## Too few all now contains all the months where we will be excluding episodes

  invalid_months <- too_few_all %>%
    dplyr::mutate(
      year = as.integer(lubridate::year(date)),
      month = lubridate::month(date)) %>%
    dplyr::group_by(site, year, month) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::filter(count >= threshold)

  validation <- episode_length_table %>%
    mutate(year = lubridate::year(epi_start_dttm),
           month = lubridate::month(epi_start_dttm)) %>%
    dplyr::left_join(invalid_months, by = c(
      "site" = "site",
      "year" = "year",
      "month" = "month"
    )) %>%
    dplyr::select(episode_id, count) %>%
    dplyr::mutate(validity = if_else(count >= threshold, 3L, as.integer(NA))) %>%
    dplyr::filter(!is.na(validity)) %>%
    dplyr::select(episode_id) %>%
    dplyr::pull()

    validated <- episode_length_table %>%
      mutate(validity = if_else(episode_id %in% validation, 3L, validity))

  return(validated)

}


#' Validate CC-HIC Events
#'
#' Validates a CC-HIC event. This should be applied after event flagging has taken place
#'
#' @param validated_episodes a tibble with 1 column of validated episodes
#' @param flagged_data_item a tibble of a CC-HIC event with full condition flagging
#'
#' @return a character vector with event IDs for validated episodes
#'
#' @export
#'
#' @importFrom rlang !! .data
#'
#' @examples
#' validate_field(core, reference, "NIHR_HIC_ICU_0108", qref, episode_length)
validate_event <- function(validated_episodes = NULL, flagged_events = NULL) {

  x <- flagged_events %>%
    dplyr::filter(.data$range_error == 0 | is.na(.data$range_error),
                  .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
                  .data$duplicate == 0 | is.na(.data$duplicate)) %>%
    dplyr::select(.data$episode_id, .data$internal_id) %>%
    dplyr::inner_join(validated_episodes %>%
                        filter(.data$validity == 0), by = "episode_id") %>%
    dplyr::select(.data$internal_id)

  return(x)

}
