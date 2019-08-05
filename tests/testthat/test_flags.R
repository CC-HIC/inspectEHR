library(inspectEHR)
context("correct flagging")

# load mandatory data
load("/Users/edward/Documents/GitHub/inspectEHR/tests/test_data.RData")
flag_real_1d <- flag_all(test_real_1d, test_episode_length)
flag_real_2d <- flag_all(test_real_2d, test_episode_length)
flag_integer_1d <- flag_all(test_integer_1d, test_episode_length)
flag_integer_2d <- flag_all(test_integer_2d, test_episode_length)
flag_string_1d <- flag_all(test_string_1d, test_episode_length)
flag_string_2d <- flag_all(test_string_2d, test_episode_length)
flag_time_1d <- flag_all(test_time_1d, test_episode_length)
flag_datetime_1d <- flag_all(test_datetime_1d, test_episode_length)
flag_date_1d <- flag_all(test_date_1d, test_episode_length)

test_that("classes are preserved", {
  expect_is(flag_real_1d, "real_1d")
  expect_is(flag_real_2d, "real_2d")
  expect_is(flag_integer_1d, "integer_1d")
  expect_is(flag_integer_2d, "integer_2d")
  expect_is(flag_string_1d, "string_1d")
  expect_is(flag_string_2d, "string_2d")
  expect_is(flag_time_1d, "time_1d")
  expect_is(flag_datetime_1d, "datetime_1d")
  expect_is(flag_date_1d, "date_1d")
})

test_that("code names are preserved", {
  expect_equal(attr(flag_real_1d, "code_name"), "NIHR_HIC_ICU_0017")
  expect_equal(attr(flag_real_2d, "code_name"), "NIHR_HIC_ICU_0132")
  expect_equal(attr(flag_integer_1d, "code_name"), "NIHR_HIC_ICU_0087")
  expect_equal(attr(flag_integer_2d, "code_name"), "NIHR_HIC_ICU_0108")
  expect_equal(attr(flag_string_1d, "code_name"), "NIHR_HIC_ICU_0058")
  expect_equal(attr(flag_string_2d, "code_name"), "NIHR_HIC_ICU_0126")
  expect_equal(attr(flag_time_1d, "code_name"), "NIHR_HIC_ICU_0043")
  expect_equal(attr(flag_datetime_1d, "code_name"), "NIHR_HIC_ICU_0411")
  expect_equal(attr(flag_date_1d, "code_name"), "NIHR_HIC_ICU_0033")
})

test_that("flagged tables are all the same length", {
  expect_length(flag_real_1d, 9)
  expect_length(flag_real_2d, 10)
  expect_length(flag_integer_1d, 9)
  expect_length(flag_integer_2d, 10)
  expect_length(flag_string_1d, 9)
  expect_length(flag_string_2d, 10)
  expect_length(flag_time_1d, 9)
  expect_length(flag_datetime_1d, 9)
  expect_length(flag_date_1d, 9)
})

test_names_1d <- c("internal_id", "site", "code_name", "episode_id",
                "value", "range_error", "out_of_bounds",
                "duplicate", "periodicity")

test_names_2d <- c("episode_id", "periodicity", "internal_id", "site",
                   "code_name", "datetime", "value", "range_error", "out_of_bounds",
                   "duplicate")

test_that("flagged tables have the correct naming scheme", {
  expect_equal(names(flag_real_1d), test_names_1d)
  expect_equal(names(flag_real_2d), test_names_2d)
  expect_equal(names(flag_integer_1d), test_names_1d)
  expect_equal(names(flag_integer_2d), test_names_2d)
  expect_equal(names(flag_string_1d), test_names_1d)
  expect_equal(names(flag_string_2d), test_names_2d)
  expect_equal(names(flag_time_1d), test_names_1d)
  expect_equal(names(flag_datetime_1d), test_names_1d)
  expect_equal(names(flag_date_1d), test_names_1d)
})

test_that("real_1d handled correctly", {
  expect_equal(flag_real_1d[1, "range_error", drop = TRUE], 104)
  expect_equal(flag_real_1d[2, "range_error", drop = TRUE], 103)
  expect_true(all(is.na(flag_real_1d$out_of_bounds)))
  expect_true(all(is.na(flag_real_1d$periodicity)))
})

test_that("real_2d handled correctly", {
  expect_equal(flag_real_2d[1, "range_error", drop = TRUE], 104)
  expect_equal(flag_real_2d[2, "range_error", drop = TRUE], 103)
})

test_that("integer_1d handled correctly", {
  expect_equal(flag_integer_1d[1, "range_error", drop = TRUE], 104)
  expect_equal(flag_integer_1d[2, "range_error", drop = TRUE], 103)
  expect_true(all(is.na(flag_integer_1d$out_of_bounds)))
  expect_true(all(is.na(flag_integer_1d$periodicity)))
})


test_that("integer_2d handled correctly", {
  expect_equal(flag_integer_2d[1, "range_error", drop = TRUE], 104)
  expect_equal(flag_integer_2d[2, "range_error", drop = TRUE], 103)
})


test_that("string_1d handled correctly", {
  expect_equal(flag_string_1d[1, "range_error", drop = TRUE], 105)
  expect_equal(flag_string_1d[4, "range_error", drop = TRUE], as.integer(NA))
  expect_true(all(is.na(flag_string_1d$out_of_bounds)))
  expect_true(all(is.na(flag_string_1d$periodicity)))
  expect_equal(sum(flag_string_1d$duplicate), 0)
})

test_that("string_2d handled correctly", {
  expect_equal(flag_string_2d[4, "out_of_bounds", drop = TRUE], 102)
  #expect_equal(flag_string_2d[5, "out_of_bounds", drop = TRUE], 101)
  expect_equal(flag_string_2d[8, "out_of_bounds", drop = TRUE], as.integer(NA))
  expect_equal(flag_string_2d[9, "out_of_bounds", drop = TRUE], as.integer(NA))
  expect_equal(flag_string_2d[8, "range_error", drop = TRUE], 105)
  expect_equal(flag_string_2d[9, "range_error", drop = TRUE], 105)
})

# Tests need to be written for time_1d

test_that("date_1d handled correctly", {
  expect_equal(flag_date_1d[1, "range_error", drop = TRUE], 103)
  expect_equal(flag_date_1d[2, "range_error", drop = TRUE], 104)
  expect_true(all(is.na(flag_date_1d$out_of_bounds)))
  expect_true(all(is.na(flag_date_1d$periodicity)))
  expect_equal(sum(flag_date_1d$duplicate), 0)
})

# Tests need to be written for datetime_1d
