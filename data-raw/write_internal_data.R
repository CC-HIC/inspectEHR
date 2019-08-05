## qref

# you will need to modify this to the right path.
qref <- read_csv(file = "./data/dq_ref.csv")

## hic classes

preserved_classes <- makeDict(metadata) %>%
  mutate(class = paste(primary_column, type, sep = "_")) %>%
  distinct(class) %>%
  pull

devtools::use_data(qref, preserved_classes, internal = FALSE, overwrite = TRUE)

## determining which have categorical property

hic_schema <- read_xml("data/NHIC_ICU_v8.3.2-Final.xsd")

simple_type <- xml_find_all(hic_schema, xpath = "//xs:simpleType")

categorical_hic <- sapply(X = simple_type, FUN = xml_attr, attr = "name") %>%
  str_sub(end = -12L)

devtools::use_data(qref, categorical_hic, preserved_classes, internal = TRUE, overwrite = TRUE)
