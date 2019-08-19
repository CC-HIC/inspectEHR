#' CC-HIC Data Quality Report
#'
#' Produce a data quality report for CC-HIC. This wraps all other functions as
#' necessary to evaluate the whole database in one sweep. This can take some
#' time!
#'
#' Specify the output folder in \code{output_folder}.
#'
#' @param database the database name for connection
#' @param username the database username
#' @param password the database password
#' @param host the database server host IP address
#' @param port the database port number
#' @param sqlite_file the filename for the sqlite database (if using it)
#' @param output_folder the path name for output files
#' @param write_plots true/false to output plots. Otherwise only data processing
#'   takes place, which can be considerably quicker.
#'
#' @importFrom dplyr select arrange pull left_join distinct collect tibble
#'   anti_join mutate
#' @importFrom ggplot2 ggplot aes geom_tile theme element_blank element_rect
#'   ylab ggtitle ggsave dup_axis scale_x_discrete
#' @importFrom DBI dbWriteTable
#' @importFrom rlang inform abort
#' @importFrom glue glue
#' @importFrom scales viridis_pal
#'
#' @export
#'
#' @examples
#' \dontrun{
#' report(sqlite_file = "./data-raw/synthetic_db.sqlite3",
#'        output_folder = "./outputs")
#' }
report <- function(database = NULL,
                   username = NULL,
                   password = NULL,
                   host = "localhost",
                   port = 5432,
                   sqlite_file = NULL,
                   output_folder = NULL,
                   write_plots = TRUE,
                   generate_report = TRUE) {

  stopifnot(
    all(!is.null(database), !is.null(username), !is.null(password)) ||
      !is.null(sqlite_file)
  )

  # Folder set up ====
  if (str_sub(output_folder, -1) != "/") {
    output_folder <- paste0(output_folder, "/")
  }

  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
    inform(glue("Path: {output_folder} does not exist. Creating directory"))
  }

  if (!dir.exists(paste0(output_folder, "plots"))) {
    dir.create(paste0(output_folder, "plots"))
  }
  if (!dir.exists(paste0(output_folder, "data"))) {
    dir.create(paste0(output_folder, "data"))
  }

  inform("Starting Episode Evaluation")

  hic_codes <- qref %>%
    dplyr::select(code_name) %>%
    dplyr::arrange(code_name) %>%
    dplyr::pull()

  for (i in seq_along(hic_codes)) {
    if (!dir.exists(paste0(output_folder, "plots/", hic_codes[i]))) {
      dir.create(paste0(output_folder, "plots/", hic_codes[i]))
    }
  }

  if (!dir.exists(paste0(output_folder, "plots/episodes"))) {
    dir.create(paste0(output_folder, "plots/episodes"))
  }

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

  # Collect small tables
  episodes <- collect(tbls[["episodes"]])
  provenance <- collect(tbls[["provenance"]])
  metadata <- collect(tbls[["variables"]])

  ## Missing fields
  # we want to identify fields that are not contributed by a site
  # so we must retrive the unique events that each site contributes first
  unique_events <- tbls[["events"]] %>%
    left_join(tbls[["episodes"]], by = "episode_id") %>%
    left_join(tbls[["provenance"]], by = c("provenance" = "file_id")) %>%
    select(site, code_name) %>%
    distinct() %>%
    collect() %>%
    mutate(contributed = "Yes")

  # Capture all the sites currently contributing to the project
  all_sites <- provenance %>%
    dplyr::select(site) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # Make a new data frame with codes replicated for each site
  all_events <- dplyr::tibble(
    site = rep(all_sites, each = nrow(qref)),
    code_name = rep(hic_codes, length(all_sites))
  )

  # use anti_join to find which sites aren't providing certain codes
  missing_events <- full_join(
    all_events, unique_events,
    by = c(
      "site" = "site",
      "code_name" = "code_name"
    )
  ) %>%
    left_join(qref %>%
                select(code_name, short_name),
              by = "code_name") %>%
    dplyr::mutate(
      new_name = paste0(
        str_sub(code_name, -4, -1), ": ", str_trunc(short_name, 20)
      )
    ) %>%
    mutate(contributed = if_else(is.na(contributed), "No", contributed)) %>%
    filter(!is.na(site))

  # make a plot highlighting missing data
  missing_events_plot <- missing_events %>%
    ggplot(aes(x = site, y = new_name, fill = contributed)) +
    geom_point(shape = 21, size = 4.5) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank()
    ) +
    ylab("Code and Name of Data Item") +
    xlab("Site")

  ggsave(
    filename = paste0(
      output_folder,
      "plots/",
      "missing_events.png"
    ),
    plot = missing_events_plot,
    height = 40, width = 5
  )

  # Useful Tables
  core <- make_core(connection = ctn)
  reference <- make_reference(connection = ctn)

  ## Capture colour profile for consistency
  all_sites.col <- viridis_pal()(length(all_sites))
  names(all_sites.col) <- all_sites

  # Cases ----
  # Gives a tibble of admission numbers (patients/episodes) by week
  admissions_weekly <- reference %>%
    dplyr::distinct() %>%
    weekly_admissions()

  # Gives overall admission numbers (totals) for patients/episodes
  admissions_by_unit <-
    unit_admissions(
      events_table = tbls[["events"]],
      reference_table = reference
    )

  for (i in seq_along(all_sites)) {
    temp_heat <- make_heatcal(reference, type = "episodes", site = all_sites[i])
    temp_plot <- plot(temp_heat, display = FALSE)
    ggsave(
      temp_plot,
      filename = paste0(
        output_folder, "plots/episodes/", all_sites[i], "_admissions.svg"
      ), height = 8, width = 12.88
    )
  }

  # Length of Stay "Episode Length" ----
  # Epsiode_length
  episode_length <- characterise_episodes(ctn)
  spell_length <- characterise_spells(episode_length)

  # Episode validity long term average
  valid_episodes <- validate_episodes(episode_length = episode_length)

  # Write out this validation to the database
  episode_validation <- valid_episodes %>%
    select(episode_id, validity)

  copy_to(ctn, episode_validation, temporary = FALSE, overwrite = TRUE)

  rlang::inform("Finished episode evaluation")
  rlang::inform("Starting event evaluation")

  # Set up events
  hic_event_summary <- vector(mode = "list", length = length(hic_codes))
  names(hic_event_summary) <- hic_codes

  for (i in seq_along(hic_codes)) {

    # The basics...
    temp_df <- extract(core_table = core, input = hic_codes[i]) %>%
      flag_all(episode_length)

    # The plotting
    if (write_plots) {
      plot_hic(
        x = temp_df,
        output_folder = paste0(output_folder, "plots/", hic_codes[i]),
        all_sites.col = all_sites.col
      )
    }

    # Saving errors outside the main list
    try(hic_event_summary[[hic_codes[i]]] <- summary_main(temp_df, reference))

    hic_event_validation <- validate_event(validated_episodes, temp_df)

    if (nrow(hic_event_validation) > 0) {
      DBI::dbWriteTable(conn = ctn, name = "event_validation", value = hic_event_validation, append = TRUE)
    }

    rlang::inform(glue("Finished validating: {hic_codes[i]}"))
  }

  rlang::inform("Finished event level evaluation")

  save(hic_event_summary,
    file = paste0(
      output_folder,
      "data/hic_event_summary.RData"
    )
  )

  # close the connection
  DBI::dbDisconnect(ctn)
}
