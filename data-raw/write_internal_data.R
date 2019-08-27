## Data Quality Reference

# you will need to modify this to the right path.
qref <- readr::read_csv(file = "./data/dq_ref.csv")
load("./data-raw/resources/metadata_tbl.RData")
load("./data-raw/resources/possible_values.RData")

qref <- dplyr::left_join(qref, qref_pv, by = "code_name")

# CC-HIC classes
preserved_classes <- make_dict(variables_tbl) %>%
  mutate(class = paste(primary_column, type, sep = "_")) %>%
  distinct(class) %>%
  pull()

## CC-HIC categorical
hic_schema <- xml2::read_xml("data/NHIC_ICU_v8.3.2-Final.xsd")
simple_type <- xml2::xml_find_all(hic_schema, xpath = "//xs:simpleType")
categorical_hic <- sapply(X = simple_type, FUN = xml2::xml_attr, attr = "name") %>%
  str_sub(end = -12L)

usethis::use_data(
  qref, categorical_hic, preserved_classes, internal = TRUE, overwrite = TRUE)

