context("Extract Demographics (1d-data)")
library(inspectEHR)

db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
ctn <- connect(sqlite_file = db_pth)
core <- make_core(ctn)
hic_codes <- "NIHR_HIC_ICU_0409"
new_labels <- "apache_score"
dtb <- extract_demographics(ctn, episode_ids = 13639:13643, hic_codes, new_labels)
DBI::dbDisconnect(ctn)

test_that("Table properties are correct", {
  expect_true(class(dtb)[1] == "1-dim")
  expect_equal(dim(dtb), c(5, 2))
  expect_equal(names(dtb), c("episode_id", "apache_score"))
  expect_true(class(attr(dtb, "lookups"))[1] == "tbl_df")
  expect_equal(dim(attr(dtb, "lookups")), c(1, 2))
  expect_equal(names(attr(dtb, "lookups")), c("codes", "names"))
})


