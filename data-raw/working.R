#' ## DB Connection
#' ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
#'
#' ## Pre-requisites
#' core <- make_core(ctn)
#' episode_length <- characterise_episodes(ctn)
#'
#' ## Data item extraction
#' hr <- extract(core, input = "NIHR_HIC_ICU_0108")
#'
#' ## Full varification
#' vhr <- varify_events(hr, episode_length)
#' head(vhr)
library(devtools); load_all()
ctn <- connect(sqlite_file = "./data-raw/synthetic_db.sqlite3")
#ctn <- connect(sqlite_file = "~/_data/hic/public/full_synthetic_db.sqlite3")
core <- make_core(ctn)
reference <- make_reference(ctn)

episode_length <- characterise_episodes(ctn)
verified_episodes <- verify_episodes(episode_length)

str_1d <- extract(core, input = "NIHR_HIC_ICU_0093") %>% verify_events(verified_episodes)
int_1d <- extract(core, input = "NIHR_HIC_ICU_0010") %>% verify_events(verified_episodes)
dbl_1d <- extract(core, input = "NIHR_HIC_ICU_0017") %>% verify_events(verified_episodes)
dt_1d <- extract(core, input = "NIHR_HIC_ICU_0033") %>% verify_events(verified_episodes)
tm_1d <- extract(core, input = "NIHR_HIC_ICU_0043") %>% verify_events(verified_episodes)
dttm_1d <- extract(core, input = "NIHR_HIC_ICU_0411") %>% verify_events(verified_episodes)
int_2d <- extract(core, input = "NIHR_HIC_ICU_0108") %>% verify_events(verified_episodes)
dbl_2d <- extract(core, input = "NIHR_HIC_ICU_0122") %>% verify_events(verified_episodes)
str_2d <- extract(core, input = "NIHR_HIC_ICU_0126") %>% verify_events(verified_episodes)

ks_test(dt_1d)

report(sqlite_file = "./data-raw/synthetic_db.sqlite3",
       output_folder = "~/Documents/academic/cc-hic/dq-demo")
