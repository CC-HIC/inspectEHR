relative_time.hic_dbl <- function(x, los_table = NULL) {

  x %<>%
    dplyr::left_join(los_table %>%
                       select(episode_id, epi_start_dttm),
                     by = "episode_id") %>%
    dplyr::mutate(reltime = difftime(epi_start_dttm, datetime, units = "hours")) %>%
    dplyr::select(-epi_start_dttm)

  class(x) <- append(class(x), "hic_dbl")

  return(x)

}
