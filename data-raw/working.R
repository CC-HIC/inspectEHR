#' ## DB Connection
#' ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#' episode_length <- characterise_episodes(ctn)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## Full varification
#' vhr <- varify_events(hr, episode_length)
#' head(vhr)

ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
ctn <- connect(sqlite_file = "~/_data/hic/public/full_synthetic_db.sqlite3")
core <- make_core(ctn)
episode_length <- characterise_episodes(ctn)
verified_episodes <- verify_episodes(episode_length)

hr <- extract(core, input = "NIHR_HIC_ICU_0108")
vhr <- varify_events(hr, episode_length)

report(sqlite_file = "./data-raw/synthetic_db.sqlite3",output_folder = "~/Documents/academic/cc-hic/dq-demo")

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
    too_few = if_else(
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
  mutate(
    site = rep(
      unique(episode_length$site),
      each = nrow(ds) / length(unique(episode_length$site))
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
  filter(count >= 10)

verification <- episode_length %>%
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
  mutate(veracity = if_else(is.na(count), 0L, 1L)) %>%
  select(episode_id, veracity) %>%
  right_join(episode_length, by = "episode_id") %>%
  filter(veracity == 1)

return(verification)
}

