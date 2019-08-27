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
library(devtools); load_all()
ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
#ctn <- connect(sqlite_file = "~/_data/hic/public/full_synthetic_db.sqlite3")
core <- make_core(ctn)
reference <- make_reference(ctn)

hr_long <- extract_timevarying(ctn, "NIHR_HIC_ICU_0108")

episode_length <- characterise_episodes(ctn)
verified_episodes <- verify_episodes(episode_length)

str_1d <- extract(core, input = "NIHR_HIC_ICU_0093") %>% verify_events(verified_episodes)
int_1d <- extract(core, input = "NIHR_HIC_ICU_0010") %>% verify_events(verified_episodes)
dbl_1d <- extract(core, input = "NIHR_HIC_ICU_0017") %>% verify_events(verified_episodes)
dt_1d <- extract(core, input = "NIHR_HIC_ICU_0033") %>% verify_events(verified_episodes)
tm_1d <- extract(core, input = "NIHR_HIC_ICU_0043") %>% verify_events(verified_episodes)
dttm_1d <- extract(core, input = "NIHR_HIC_ICU_0411") %>% verify_events(verified_episodes)
int_2d <- extract(core, input = "NIHR_HIC_ICU_0108") %>% verify_events(verified_episodes)
dbl_2d <- extract(core, input = "NIHR_HIC_ICU_0116") %>% verify_events(verified_episodes)
str_2d <- extract(core, input = "NIHR_HIC_ICU_0126") %>% verify_events(verified_episodes)


plot(make_heatcal(reference, str_1d, site = "A"))

## expand to ensure the full coverage is from the first month first year
## to last month last year for all sites.
## Then look at the long running average (? same methodology as for valid months)
## And mark when there is a period of unacceptably low contribution


# The typical submissions for a given day of the week within a seasons of the
# year. This helps to account for seasonality and trend changes over time
base_events <- str_1d %>%
  filter(
    .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
    .data$range_error == 0L | is.na(.data$range_error),
    .data$duplicate == 0L | is.na(.data$duplicate)
  ) %>%
  left_join(
    reference %>% select(-site),
    by = "episode_id") %>%
  mutate(date = lubridate::as_date(start_date)) %>%
  select(site, event_id, date) %>%
  group_by(site, date) %>%
  summarise(event_count = n_distinct(event_id)) %>%
  mutate(
    year = lubridate::year(date),
    season = lubridate::quarter(date, with_year = FALSE, fiscal_start = 3),
    wday = lubridate::wday(date, label = TRUE)
  )

base_calendar <- base_events %>%
  group_by(site) %>%
  summarise(start = lubridate::floor_date(min(date), unit = "year"),
            end = lubridate::ceiling_date(max(date), unit = "year")-1) %>%
  tidyr::nest(start, end, .key = "date") %>%
  mutate(date = purrr::map(date, ~ seq.Date(.x$start, .x$end, by = "day"))) %>%
  unnest(date)

typical_events <- base_events %>%
  group_by(site, year, season, wday) %>%
  summarise(
    mean_events = mean(event_count),
    sd_events = sd(event_count)
  )

  # too_few = days when events fell under the expectation
too_few <- base_events %>%
  left_join(typical_events, by = c(
    "site", "year", "season", "wday"
  )) %>%
  mutate(
    too_few = if_else(
      event_count < (mean_events - 2 * sd_events), TRUE, FALSE
    )
  ) %>%
  filter(too_few == TRUE) %>%
  select(site, date)

# what we don't capture is days when there is no data - i.e. NAs
# this is what we will fix here
na_days <- reference %>%
  mutate(date = lubridate::date(start_date)) %>%
  select(site, date) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(admission = TRUE)

ds <- tibble(
  date = rep(
    seq.Date(
      from = min(lubridate::date(reference$start_date)),
      to = max(lubridate::date(reference$start_date)),
      by = "day"
    ),
    times = length(unique(reference$site))
  )
)

ds <- ds %>%
  mutate(
    site = rep(
      unique(reference$site),
      each = nrow(ds) / length(unique(reference$site))
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

coverage_bad <- too_few_all %>%
  mutate(
    year = as.integer(lubridate::year(date)),
    month = lubridate::month(date)
  ) %>%
  group_by(site, year, month) %>%
  summarise(count = n()) %>%
  filter(count >= 10)

verification <- reference %>%
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
  right_join(episode_length, by = "episode_id")


  dplyr::left_join(cases_all_tbl,
                   by = c(
                     "site" = "site",
                     "year" = "year",
                     "month" = "month",
                     "week_of_month" = "week_of_month"
                   )
  ) %>%
  dplyr::select(site, date, year, month, week_of_month, wday, count, episodes, patients, est_occupancy) %>%
  dplyr::mutate(disparity = ifelse(is.na(count), NA, count / est_occupancy))


summary_main(str_1d)

df <- ks_test(dbl_2d)

plot_ks(df, reference)


report(sqlite_file = "./data-raw/synthetic_db.sqlite3",output_folder = "~/Documents/academic/cc-hic/dq-demo")

