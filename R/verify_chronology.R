#' Verify Chronology
#'
#' @param connection a database connection
#'
#' @return
#' @export
verify_chronology <- function(connection){

  chrono_codes <- tribble(
    ~codes, ~order,
    "NIHR_HIC_ICU_0033", "a", #DoB
    "NIHR_HIC_ICU_0032", "b", #Admission to Hosp
    "NIHR_HIC_ICU_0411", "c", #Arrival in ICU
    "NIHR_HIC_ICU_0050", "d", #Ready for discharge
    "NIHR_HIC_ICU_0048", "e", #Withdraw
    "NIHR_HIC_ICU_0412", "f", #Discharge
    "NIHR_HIC_ICU_0042", "g", #Death
    "NIHR_HIC_ICU_0045", "h", #BSD
    "NIHR_HIC_ICU_0038", "i", #Body
    "NIHR_HIC_ICU_0406", "j" #Hospital Discharge
    )

  dtb <- extract_demographics(
    connection = connection,
    code_names = chrono_codes$codes,
    rename = chrono_codes$order)

  ## Turn everything into just dates
  ## Check that everything reads left to right in the order above
  ## Check all are before today
  ## Check all are after the birthday

  ## Need to check that gather pulls everyting into the correct order first
  corr_order <- quos(chrono_codes$order)

  dtb %>%
    select(episode_id, !!!corr_order) %>%
    mutate_at(.vars = vars(c, f),
              .funs = function(x) {
                lubridate::as_date(lubridate::ymd_hms(x))
              }) %>%
    group_by(episode_id) %>%
    mutate_if(.predicate = is.character, .funs = lubridate::ymd) %>%
    gather(key = "code", value = "date", -episode_id) %>%
    arrange(episode_id, code, date) %>%
    mutate(in_series = if_else(
      date <= lead(date) | is.na(date) | is.na(lead(date)), TRUE, FALSE))

}
