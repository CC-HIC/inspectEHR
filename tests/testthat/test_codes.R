library(inspectEHR)
context("Post Codes")

test_that("legitimate postcodes are correctly identified", {
  expect_true(validate_post_code("M20 2LL"))
  expect_true(validate_post_code("BH20 6EE"))
})

test_that("incorrect postcodes are correctly identified", {
  expect_false(validate_post_code("sdf 32f"))
})


context("ICNARC Codes")

test_that("correct ICNARC code", {
  expect_true(validate_icnarc("1.1.1.1.1"))
})
