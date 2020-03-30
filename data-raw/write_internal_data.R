## Data Quality Reference
# you will need to modify this to the right path.
mdata <- readr::read_csv("./inst/extdata/omop_mapping_ref.csv")
usethis::use_data(mdata, internal = TRUE, overwrite = TRUE)

