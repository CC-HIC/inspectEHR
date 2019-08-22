context("Varify Events")
library(inspectEHR)

ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
episode_length <- characterise_episodes(ctn)
verified_episodes <- verify_episodes(episode_length)

## Testing 1 of each datatype present in the test DB
nhs <- extract(core, input = "NIHR_HIC_ICU_0078") # String-1d
hr <- extract(core, input = "NIHR_HIC_ICU_0108") # Real-2d

vhr <- verify_events(hr, los_table = verified_episodes)

rhr <- verify_range(hr)
bhr <- verify_bounds(hr, verified_episodes)
dhr <- verify_duplicate(hr)

ahr <- reduce(list(hr, rhr, bhr, dhr), left_join, by = "event_id")

verify_periodicity(ahr, verified_episodes)

ahr %>%
  # filter out values that cannot be taken into consideration for this
  # calculation
  filter(
    .data$out_of_bounds == 0L | is.na(.data$out_of_bounds),
    .data$range_error == 0L | is.na(.data$range_error),
    .data$duplicate == 0L | is.na(.data$duplicate)) %>%
  group_by(.data$episode_id) %>%
  tally() %>%
  left_join(los_table %>%
              # only checking validated episodes
              filter(.data$veracity == 0L) %>%
              select(.data$episode_id, .data$los_days),
            by = "episode_id"
  ) %>%
  # calculate the periodicity
  mutate(periodicity = count / as.numeric(los_days)) %>%
  select(.data$episode_id, .data$periodicity) %>%

  # right join back into the original object
  # this will produce NAs on the following conditions: invalid LOS or no
  # usable values
  right_join(x, by = "episode_id")

quan <- quantile(x$periodicity,
                 na.rm = TRUE,
                 probs = c(0.05, 0.95),
                 names = FALSE)

x <- x %>%
  mutate(var_per = case_when(
    is.na(periodicity) ~ as.integer(NA),
    periodicity < probs[1] ~ -1L,
    periodicity > probs[2] ~ 1L,
    TRUE ~ 0L
  ))
}

test_that("varified ", {
  expect_true(verify_post_code("M20 2LL"))
  expect_true(verify_post_code("BH20 6EE"))
})


