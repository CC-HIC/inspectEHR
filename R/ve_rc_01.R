#' Validate VE_RC_01: Referential integrity between tables
#'
#' @param tbls
#'
#' @return
#' @export
#'
#' @examples
ve_rc_01 <- function(tbls) {
  bind_rows(
    tally(
      anti_join(tbls[["episodes"]],
                tbls[["provenance"]], by = c("provenance" = "file_id"))) %>%
      collect(),
    tally(
      anti_join(tbls[["provenance"]],
                tbls[["episodes"]], by = c("file_id" = "provenance"))) %>%
      collect(),
    tally(
      anti_join(tbls[["episodes"]],
                tbls[["events"]], by = "episode_id")) %>%
      collect(),
    tally(
      anti_join(tbls[["events"]],
                tbls[["episodes"]], by = "episode_id")) %>%
      collect(),
    tally(
      anti_join(tbls[["events"]],
                tbls[["variables"]], by = "code_name")) %>%
      collect(),
    tally(
      anti_join(tbls[["variables"]],
                tbls[["events"]], by = "code_name")) %>%
      collect()
  ) %>%
    add_column(conformity_check = c(
      "episodes not in provenance",
      "provenance not in episodes",
      "episodes not in events",
      "events not in episodes",
      "events not in variables",
      "variables not in events"
    ), .before = TRUE)
}
