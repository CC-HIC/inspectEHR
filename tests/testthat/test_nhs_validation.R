library(inspectEHR)
context("NHS Numbers")

test_that("incorrect lengths are handled", {
  expect_error(validate_nhs(123))
})

test_that("incorrect data types are handled", {
  expect_error(validate_nhs(1234567890))
  expect_error(validate_nhs(c(1234567890, 0987654321)))
})

test_that("nhs numbers validate correctly", {
  expect_equal(validate_nhs("6744424270"), TRUE)
  expect_equal(validate_nhs(c("6744424270", "2925091350")), c(TRUE, TRUE))
  expect_equal(validate_nhs(c("6744424270", "0083041737", "2401060723")), c(TRUE, TRUE, TRUE))
  expect_equal(validate_nhs("1234567890"), FALSE)
  expect_equal(validate_nhs(c("6744424270", "1234567890")), c(TRUE, FALSE))
  expect_equal(validate_nhs(c("6744424270", "0083041737", "1234567890")), c(TRUE, TRUE, FALSE))
})
