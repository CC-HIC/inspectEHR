#' Plot Patient
#'
#' @param connection
#' @param nhs_number
#'
#' @importFrom dplyr filter select collect pull mutate group_by summarise
#' @importFrom ggplot2 ggplot aes geom_dotplot
#'
#' @return
#' @export
#'
#' @examples
plot_patient <- function(connection, nhs_number) {

  value_title <- "Patient Overview"
  subtitle <- nhs_number
  x_lab <- "time"
  y_lab <- "event count"

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

  epi_ids <- tbl(connection, "events") %>%
    filter(.data$string == nhs_number) %>%
    select(.data$episode_id) %>%
    collect() %>%
    pull()

  df <- tbl(connection, "events") %>%
    filter(.data$episode_id %in% epi_ids) %>%
    select(.data$code_name, .data$datetime,
           .data$date, .data$time) %>%
    collect()

  dfs <- df %>%
    mutate(date = lubridate::as_date(datetime))

  dot_plot <- dfs %>%
    ggplot(
      aes(x = .data$datetime)) +
    geom_rug() +
    ylab(y_lab) +
    xlab(x_lab) +
    ggtitle(value_title, subtitle = subtitle) +
    theme_cchic()

  invisible(dot_plot)
}
