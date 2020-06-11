ve_rc_02 <- function(tbls) {

  tibble(
    table = c("episodes", "events", "provenance"),
    keys_unique =
  c(collect(tally(tbls[["episodes"]]))$n ==
    collect(tally(distinct(tbls[["episodes"]], episode_id)))$n,
  collect(tally(tbls[["events"]]))$n ==
    collect(tally(distinct(tbls[["events"]], event_id)))$n,
  collect(tally(tbls[["provenance"]]))$n ==
    collect(tally(distinct(tbls[["provenance"]], file_id)))$n)
  )

}
