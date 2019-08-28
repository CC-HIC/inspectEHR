context("Varify Events")
library(inspectEHR)

db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
ctn <- connect(sqlite_file = db_pth)
core <- make_core(ctn)
# episode_length <- characterise_episodes(ctn)
# verified_episodes <- verify_episodes(episode_length)

## Testing 1 of each datatype present in the test DB
str_1d <- extract(core, input = "NIHR_HIC_ICU_0073") # string-1d
int_1d <- extract(core, input = "NIHR_HIC_ICU_0010") # AML - integer-1d
dbl_1d <- extract(core, input = "NIHR_HIC_ICU_0017") # Height - real-1d
dt_1d <- extract(core, input = "NIHR_HIC_ICU_0033") # DoB - date-1d
tm_1d <- extract(core, input = "NIHR_HIC_ICU_0043") # Deathtime - time-1d
dttm_1d <- extract(core, input = "NIHR_HIC_ICU_0411") # Admission - datetime-1d
int_2d <- extract(core, input = "NIHR_HIC_ICU_0108") # integer-2d
dbl_2d <- extract(core, input = "NIHR_HIC_ICU_0116") # cvp real-2d
str_2d <- extract(core, input = "NIHR_HIC_ICU_0126") # airway string-2d

test_that("events are extracted to the correct data type", {
  expect_true(class(str_1d$value) == "character")
  expect_true(class(int_1d$value) == "integer")
  expect_true(class(dbl_1d$value) == "numeric")
  expect_true(class(dt_1d$value) == "Date")
  expect_true(class(tm_1d$value)[1] == "hms")
  expect_true(class(dttm_1d$value)[1] == "POSIXct")
  expect_true(class(str_2d$value) == "character")
  expect_true(class(int_2d$value) == "integer")
  expect_true(class(dbl_2d$value) == "numeric")
})

test_that("events carry the correct class", {
  expect_true(class(str_1d)[1] == "string_1d")
  expect_true(class(int_1d)[1] == "integer_1d")
  expect_true(class(dbl_1d)[1] == "real_1d")
  expect_true(class(dt_1d)[1] == "date_1d")
  expect_true(class(tm_1d)[1] == "time_1d")
  expect_true(class(dttm_1d)[1] == "datetime_1d")
  expect_true(class(str_2d)[1] == "string_2d")
  expect_true(class(int_2d)[1] == "integer_2d")
  expect_true(class(dbl_2d)[1] == "real_2d")
})


