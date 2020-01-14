#' Verify Patients based upon Data Item Requirements
#'
#' Interrogates the CC-HIC database and provides a vector of episode_ids and event_ids that
#' can "safely" be used for your study based on the dataitems and time restrictions you require. This
#' can then be plugged into extract_demographics or extract_timevarying to pull out the study data you
#' require. Each validation setting can be controlled independently.
#'
#' @importFrom tidyr expand_grid
#' @importFrom purrr compact reduce map
#'
#' @return
#' @export
#'
#' @examples
verify_study <- function(
  code_names, from = NULL, to = NULL,
  database = NULL,
  username = NULL,
  password = NULL,
  host = "localhost",
  port = 5432,
  sqlite_file = NULL,
  out_of_bounds = TRUE, range_error = TRUE, duplicate = TRUE,
  coverage = TRUE
  ) {

  stopifnot(
    all(!is.null(database), !is.null(username), !is.null(password)) ||
      !is.null(sqlite_file)
  )
  
  if (!all(code_names %in% qref$code_name)) {
    print(code_names[!(code_names %in% qref$code_name)])
    rlang::abort("You are trying to evaluate for data items not contained in HIC")
  }

  hic_codes <- code_names

  # Prepare Connections
  ctn <- connect(
    database = database,
    username = username,
    password = password,
    host = host,
    port = port,
    sqlite_file = sqlite_file
  )

  tbls <- retrieve_tables(ctn)
  
  if (any(names(tbls) %in% "episode_verification")) {
    upper <- max(tbls[["episode_verification"]]$datetime)
    lower <- min(tbls[["episode_verification"]]$datetime)
    mess <- paste0(
      "An existing episode verification exists from: ",
      lower,
      " to: ",
      upper
    )
    rlang::inform(mess)
    x <- readline(prompt = "Do you wish to use these verifications? 1 = Yes, Any = No, Refresh: ")
    if (x != 1) {
      episode_length <- characterise_episodes(ctn)
      spell_length <- characterise_spells(episode_length)
      # Episode validity long term average
      ve_episodes <- verify_episodes(episode_length = episode_length)
    }
  } else {
    episode_length <- characterise_episodes(ctn)
    spell_length <- characterise_spells(episode_length)
    # Episode validity long term average
    ve_episodes <- verify_episodes(episode_length = episode_length)
  }
  
  if (x == 1) {
    ve_episodes <- collect(tbls[["episode_verification"]])
  }

  # Collect small tables
  provenance <- collect(tbls[["provenance"]])
  metadata <- collect(tbls[["variables"]])

  ## Missing fields
  # we want to identify fields that are not contributed by a site
  # so we must retrive the unique events that each site contributes first
  unique_events <- tbls[["events"]] %>%
    filter(code_name %in% code_names) %>%
    left_join(tbls[["episodes"]], by = "episode_id") %>%
    left_join(tbls[["provenance"]], by = c("provenance" = "file_id")) %>%
    select(site, code_name) %>%
    mutate(contributed = "yes") %>%
    distinct() %>%
    collect()

  # Capture all the sites currently contributing to the project
  all_sites <- provenance %>%
    dplyr::select(site) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # Make a new data frame with codes replicated for each site
  all_events <- expand_grid(
    site = all_sites,
    code_name = hic_codes)

  # use anti_join to find which sites aren't providing certain codes
  missing_events <- full_join(
    all_events, unique_events, by = c("site", "code_name")) %>%
    mutate(contributed = if_else(is.na(contributed), "no", contributed))

  # Useful Tables
  core <- make_core(connection = ctn)
  reference <- make_reference(connection = ctn)

  # Set up events
  hic_events <- vector(mode = "list", length = length(hic_codes))
  names(hic_events) <- hic_codes
  
  for (i in seq_along(hic_codes)) {
    
    # Extract the event in question from the DB
    df <- extract(core_table = core, input = hic_codes[i])
    
    # Check to see if there is any data there
    if (nrow(df) == 0 || sum(is.na(df$value)) == nrow(df)) {
      rlang::inform(glue("Skipping over {hic_codes[i]} as no data present"))
    } else {
      
      # Basic Verification: range, boundary, duplcation, periodicity
      df <- verify_events(df, ve_episodes) %>%
              verify_coverage(reference_tbl = reference)
      
      v_event_ids <- df$event_id[df$coverage == 1]
      
      # Saving errors outside the main list
      hic_events[[i]] <- v_event_ids
    }
  }

  validated_events <- order(reduce(map(hic_events, ~ as.integer(na.omit(.x))), c))
  
  out <- list(
    validated_episodes = ve_episodes$episode_id,
    missing_events = missing_events,
    validated_events = validated_events)
  
  DBI::dbDisconnect(ctn)
  return(out)
}
