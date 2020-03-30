#' Add movement data to extracted table
#'
#' @param connection a EMAP database connection
#' @param ltb a time series table produced from \code{\link{extract}}
#'
#' @return a time series table with location data appendewd
#' @export
attach_locations <- function(connection, ltb) {
  
  tbls <- retrieve_tables(connection, "ops_dev")
  vo_ids <- unique(ltb$visit_occurrence_id)
  
  vo <- tbls[["visit_occurrence"]] %>%
    filter(visit_occurrence_id %in% vo_ids) %>%
    select(visit_occurrence_id, visit_start_datetime) %>%
    collect() %>%
    rename(vo_start = visit_start_datetime)
  
  vd <- tbls[["visit_detail"]] %>%
    filter(visit_occurrence_id %in% vo_ids) %>%
    left_join(tbls[["care_site"]] %>%
                select(care_site_id, care_site_name),
              by = "care_site_id") %>%
    select(person_id, visit_occurrence_id,
           visit_detail_id, visit_start_datetime,
           care_site_id, care_site_name) %>%
    collect() %>%
    rename(vd_start = visit_start_datetime)
  
  ltb <- left_join(vo, vd, by = "visit_occurrence_id") %>%
    mutate(time = round(
      as.numeric(
        difftime(vd_start, vo_start, units = "hours")), digits = 1)) %>%
    select(person_id, time, visit_occurrence_id, care_site_name, time) %>%
    full_join(ltb, by = c("time", "visit_occurrence_id")) %>%
    arrange(person_id, visit_occurrence_id, time)
  
  return(ltb)
}