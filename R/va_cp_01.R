#' Validate VA_CP_01: Missingness of a particular item from the requisite
# schema at site level
#'
#' @param tbls
#'
#' @return
#' @export
#'
#' @examples
va_cp_01 <- function(tbls) {
  # Capture all events, and ascertain where they came from
  unique_events <- tbls[["events"]] %>%
    left_join(tbls[["episodes"]], by = "episode_id") %>%
    left_join(tbls[["provenance"]], by = c("provenance" = "file_id")) %>%
    select(site, code_name) %>%
    distinct() %>%
    collect() %>%
    mutate(contributed = "Yes")

  # Capture all the sites currently contributing to the project
  all_sites <- provenance %>%
    select(site) %>%
    distinct() %>%
    pull()

  # Make a new container with with codes replicated for each site
  all_events <- tibble(
    site = rep(all_sites, each = nrow(qref)),
    code_name = rep(hic_codes, length(all_sites))
  )

  # use anti_join to find which sites aren't providing certain codes
  full_join(x = all_events, y = unique_events,
                              by = c(
                                "site" = "site",
                                "code_name" = "code_name")) %>%
  left_join(qref[,c("code_name", "short_name")], by = "code_name") %>%
  mutate(
    short_code = str_sub(code_name, -4, -1),
    new_name = paste0(str_sub(code_name, -4, -1), ": ", str_trunc(short_name, 20))
  ) %>%
  mutate(contributed = if_else(is.na(contributed), "No", contributed)) %>%
  filter(!is.na(site))
}
