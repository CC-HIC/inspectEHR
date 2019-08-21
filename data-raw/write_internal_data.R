## Data Quality Reference

# you will need to modify this to the right path.
qref <- read_csv(file = "./data/dq_ref.csv")

load("./data-raw/resources/metadata_tbl.RData")

# CC-HIC classes
preserved_classes <- make_dict(variables_tbl) %>%
  mutate(class = paste(primary_column, type, sep = "_")) %>%
  distinct(class) %>%
  pull()

## CC-HIC Categorical.
hic_schema <- read_xml("data/NHIC_ICU_v8.3.2-Final.xsd")
simple_type <- xml_find_all(hic_schema, xpath = "//xs:simpleType")
categorical_hic <- sapply(X = simple_type, FUN = xml_attr, attr = "name") %>%
  str_sub(end = -12L)

devtools::use_data(
  qref, categorical_hic, preserved_classes, internal = TRUE, overwrite = TRUE)
