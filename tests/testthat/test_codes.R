context("Varify Codes")
library(inspectEHR)

test_that("legitimate postcodes varify correctly", {
  expect_true(verify_post_code("M20 2LL"))
  expect_true(verify_post_code("BH20 6EE"))
})

test_that("incorrect postcodes are rejected correctly", {
  expect_false(verify_post_code("sdf 32f"))
  expect_false(verify_post_code("hippo"))
})

test_that("ICNARC codes of a variety of formats are verified correctly", {
  expect_true(verify_icnarc("1.1.1.1.1"))
  expect_true(verify_icnarc("01.01.01.01.01"))
  expect_true(verify_icnarc("01.1.01.1.01"))
  expect_true(verify_icnarc("01-01-01-01-01"))
})

test_that("incorrect ICNARC codes of a variety
          of formats are rejected correctly", {
  expect_false(verify_icnarc("1.1.1.."))
  expect_false(verify_icnarc("1.1.1.NA.NA"))
  expect_false(verify_icnarc("lemon"))
  expect_error(verify_icnarc(0101010101))
})

test_that("incorrect lengths are handled", {
  expect_error(verify_nhs(123))
})

test_that("incorrect data types are handled", {
  expect_error(verify_nhs(1234567890))
  expect_error(verify_nhs(c(1234567890, 0987654321)))
})

test_that("nhs numbers validate correctly", {
  expect_equal(verify_nhs("6744424270"), TRUE)
  expect_equal(verify_nhs(c("6744424270", "2925091350")), c(TRUE, TRUE))
  expect_equal(verify_nhs(c("6744424270", "0083041737", "2401060723")),
               c(TRUE, TRUE, TRUE))
  expect_equal(verify_nhs("1234567890"), FALSE)
  expect_equal(verify_nhs(c("6744424270", "1234567890")), c(TRUE, FALSE))
  expect_equal(verify_nhs(c("6744424270", "0083041737", "1234567890")),
               c(TRUE, TRUE, FALSE))
})
