library(devtools)
load_all()

ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")

ltb <- extract_timevarying(ctn, "NIHR_HIC_ICU_0108")
