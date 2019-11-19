#' Analyze Blood Gases
#' 
#' labells blood sampels according to whether or not they came from an arterial or venous source.
#'
#' @param pxo2 numeric vector of oxygen partial pressures
#' @param pxco2 numeric vector of co2 partial pressures
#' @param ph numeric vector of pH
#'
#' @return a character vector of either "arterial" or "venous".
#' @export
#' 
#' @importFrom tibble tibble add_column
#' @importFrom dplyr mutate if_else
analyze_bg <- function(pxo2, pxco2, ph, probability = FALSE) {
  
  mt <- tibble(pxo2, pxco2, ph) %>%
    mutate(
      pxo2_pxco2 = pxo2*pxco2,
      pxo2_ph = pxo2*ph,
      pxco2_ph = pxco2*ph,
      pxo2_pxco2_ph = pxo2*pxco2*ph
    ) %>%
    add_column(1L, .before = 1) %>%
    as.matrix()
  
  ot <- mt %*% multip
  prob <- inv_logit(ot[,1,drop = TRUE])
  if (probability) {
    return(prob)
  } else {
  if_else(prob >= 0.589, "arterial", "venous")
  }
  
}