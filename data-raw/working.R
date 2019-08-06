library(devtools)
load_all()

ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
tbls <- retrieve_tables(ctn)
ref_tbl <- make_reference(ctn)
core <- make_core(ctn)

hr <- inspectEHR::extract(core, "NIHR_HIC_ICU_0108")

class(hr)
