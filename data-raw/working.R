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
str_1d <- extract(core, input = "NIHR_HIC_ICU_0074") %>% verify_events(verified_episodes)

df <- extract(core_table = core, input = "NIHR_HIC_ICU_0108")

# Check to see if there is any data there
if (nrow(df) == 0 || sum(is.na(df$value)) == nrow(df)) {
  rlang::inform(glue("Skipping over {hic_codes[i]} as no data present"))
} else {

  # Basic Verification: range, boundary, duplcation, periodicity
  df <- verify_events(df, verified_episodes)

  # Coverage verification
  df_cov <- coverage(df, reference_tbl = reference)

  # Statistical verification
  ks_pos <- qref[qref$code_name == hic_codes[i], "dist_compare", drop = TRUE]
  if (!is.na(ks_pos) && ks_pos == "ks") {
    df_stat <- ks_test(df)
  } else {
    df_stat <- NULL
  }

  # Saving errors outside the main list
  hic_event_summary[[hic_codes[i]]] <- summarise_verification(df, df_stat, df_cov, reference)
