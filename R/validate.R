#' Validate Episodes
#'
#' Determines which months are invalid based on a low contribution of data.
#' This is based upon the long term daily average for admissions. Days that
#' fall below 2 SD of the long term mean are flagged. If more than the
#' \code{threshold} number of flagged days occur in a single month, then the
#' month is invalidated and removed from further analysis.
#'
#' This procedure removes episodes that occur during particularly sparse periods
#' (as this is likely that these months are contributing poor data) and return
#' only episodes that have a logical consistency in non-sparse months. The
#' analyst should consider if the denominator for the number of study months
#' should be changed following the use of this function.
#'
#' @param episode_length a table created by \code{\link{characterise_episodes}}
#' @param threshold threshold number of days to use in calculation
#'
#' @return a table of the same form as \code{episode_length} with an additional
#'   column indicating if the episode comes from a valid period of time.
#' @export
#'
#' @importFrom dplyr mutate group_by summarise left_join right_join n_distinct
#' distinct tibble bind_rows pull
#' @importFrom lubridate date year month wday
#'
#' @examples
#' validate_episodes(episodes)
validate_episodes <- function(episode_length = NULL,
                              threshold = 10) {

  # The typical admissions for a given day of the week within a year window
  # This helps to account for seasonality and trend changes over time
  typical_admissions <- episode_length %>%
    mutate(date = lubridate::date(epi_start_dttm)) %>%
    group_by(site, date) %>%
    summarise(episode_count = n_distinct(episode_id)) %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date, label = TRUE),
      wday = lubridate::wday(date, label = TRUE)
    ) %>%
    group_by(site, year, wday) %>%
    summarise(
      mean_episodes = mean(episode_count),
      sd_episode = sd(episode_count)
    )

  # too_few tells me the days when admissions fell under the expected
  too_few <- episode_length %>%
    mutate(date = lubridate::date(epi_start_dttm)) %>%
    group_by(site, date) %>%
    summarise(episodes = n()) %>%
    mutate(
      year = lubridate::year(date),
      wday = lubridate::wday(date, label = TRUE)
    ) %>%
    left_join(typical_admissions, by = c(
      "site" = "site",
      "year" = "year",
      "wday" = "wday"
    )) %>%
    mutate(
      too_few = ifelse(
        episodes < (mean_episodes - 2 * sd_episode), TRUE, FALSE
      )
    ) %>%
    filter(too_few == TRUE) %>%
    select(site, date)

  # what we don't capture is days when there is no data - i.e. NAs
  # this is what we will fix here
  na_days <- episode_length %>%
    mutate(date = lubridate::date(epi_start_dttm)) %>%
    select(site, date) %>%
    distinct(.keep_all = TRUE) %>%
    mutate(admission = TRUE)

  ds <- tibble(
    date = rep(
      seq.Date(
        from = min(lubridate::date(episode_length$epi_start_dttm)),
        to = max(lubridate::date(episode_length$epi_start_dttm)),
        by = "day"
      ),
      times = length(unique(episode_length$site))
    )
  )

  ds <- ds %>%
    mutate(site = rep(unique(episode_length$site),
                      each = nrow(ds) / length(unique(episode_length$site)
                                               )
                      )
           )

  too_few_all <- na_days %>%
    right_join(ds,
      by = c(
        "date" = "date",
        "site" = "site"
      )
    ) %>%
    filter(is.na(admission)) %>%
    select(-admission) %>%
    bind_rows(too_few)

  ## Too few all now contains all the months where we will be excluding episodes

  invalid_months <- too_few_all %>%
    mutate(
      year = as.integer(lubridate::year(date)),
      month = lubridate::month(date)
    ) %>%
    group_by(site, year, month) %>%
    summarise(count = n()) %>%
    filter(count >= threshold)

  validation <- episode_length %>%
    mutate(
      year = lubridate::year(epi_start_dttm),
      month = lubridate::month(epi_start_dttm)
    ) %>%
    left_join(invalid_months, by = c(
      "site" = "site",
      "year" = "year",
      "month" = "month"
    )) %>%
    select(episode_id, count) %>%
    mutate(validity = if_else(count >= threshold, 1L, 0L)) %>%
    select(episode_id, validity) %>%
    right_join(episode_length, by = "episode_id")

  return(validation)
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
    dplyr::filter(
      .data$range_error == 0 | is.na(.data$range_error),
      .data$out_of_bounds == 0 | is.na(.data$out_of_bounds),
      .data$duplicate == 0 | is.na(.data$duplicate)
    ) %>%
    dplyr::select(.data$episode_id, .data$event_id) %>%
    dplyr::inner_join(validated_episodes %>%
      filter(.data$validity == 0), by = "episode_id") %>%
    dplyr::select(.data$event_id)

  return(x)
}
