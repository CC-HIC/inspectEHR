context("Characterise episodes")
library(inspectEHR)

db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
ctn <- connect(sqlite_file = db_pth)
episodes <- characterise_episodes(ctn)
DBI::dbDisconnect(ctn)

test_that("Table properties are correct", {
  expect_true("data.frame" %in% class(episodes))
  expect_equal(dim(episodes), c(1000, 7))
  expect_equal(names(episodes),
               c("episode_id", "nhs_number", "epi_start_dttm",
                 "epi_end_dttm", "outcome", "los_days", "site"))
})


