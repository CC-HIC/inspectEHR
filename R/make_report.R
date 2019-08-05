#' Make HIC Report Data
#'
#' This is the main function for processing data from CC-HIC. It wraps all
#' other functions to evaluate the whole database in one sweep.
#' This can take some time!
#'
#' Specify the output folder in "path_name" when running this function,
#' path_name should be made availible with the following folders:
#' plots
#' working_data
#'
#' report.rmd can be run after this function has successfully completed to
#' produce a full report for CC-HIC
#'
#'
#' @param database the database name for connection
#' @param username the database username
#' @param password the database password
#' @param host the database server host IP address
#' @param port the database port number
#' @param sqlite_file the filename for the sqlite database (if using it)
#' @param path_name the path name for output files
#' @param spell_boundary_mins the time gap you are willing to allow for the
#' definition of spells. This will automatiically check against your
#' contraint +/- 60 mins so you can see how sensitive your calcuation is.
#' @param write_plots true/false to output plots. Otherwise only data processing takes place,
#' which can be considerably quicker.
#'
#' @importFrom dplyr select arrange pull left_join distinct collect tibble
#' anti_join mutate
#' @importFrom ggplot2 ggplot aes geom_tile theme element_blank element_rect
#' ylab ggtitle ggsave dup_axis scale_x_discrete
#' @importFrom DBI dbWriteTable
#'
#' @export
#'
#' @examples
#' make_report(database = "test_db", username = "abcde", password = "qwerty", path_name = "~/outputs")
#' make_report(database = "test_db", sqlite_file = "~/database.sqlite", path_name = "~/outputs")
make_report <- function(database = "lenient_dev",
                        username = "edward",
                        password = "superdb",
                        host = "localhost",
                        port = 5432,
                        sqlite_file = NULL,
                        path_name = NULL,
                        spell_boundary_mins = 60,
                        write_plots = TRUE) {

  # Folder set up ====
  if (str_sub(path_name, -1) != "/") {
    path_name <- paste0(path_name, "/")
  }

  if (!dir.exists(path_name)) {
    dir.create(path_name)
    cat("path:", paste(path_name), "does not exist. Creating directory")
  }

  if(!dir.exists(paste0(path_name,"plots"))) {
    dir.create(paste0(path_name, "plots")) }
  if(!dir.exists(paste0(path_name, "data"))) {
    dir.create(paste0(path_name, "data")) }

  print("Starting Episode Evaluation")

  hic_codes <- qref %>%
    dplyr::select(code_name) %>%
    dplyr::arrange(code_name) %>%
    dplyr::pull()

  for (i in seq_along(hic_codes)) {
    if(!dir.exists(paste0(path_name,"plots/", hic_codes[i]))) {
      dir.create(paste0(path_name, "plots/", hic_codes[i])) }
  }

  if(!dir.exists(paste0(path_name,"plots/episodes"))) {
    dir.create(paste0(path_name, "plots/episodes")) }

  # Prepare Connections
  ctn <- connect(database = database,
                 username = username,
                 password = password,
                 host = host,
                 port = port,
                 sqlite_file = sqlite_file)

  tbls <- retrieve_tables(ctn)

  # Collect small tables
  episodes <- collect(tbls[["episodes"]])
  provenance <- collect(tbls[["provenance"]])
  metadata <- collect(tbls[["variables"]])

  ## Missing fields
  # we want to identify fields that are not contributed by a site
  # so we must retrive the unique events that each site contributes first
  unique_events <- tbls[["events"]] %>%
    dplyr::left_join(tbls[["episodes"]], by = "episode_id") %>%
    dplyr::left_join(tbls[["provenance"]], by = c("provenance" = "file_id")) %>%
    dplyr::select(site, code_name) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  # Capture all the sites currently contributing to the project
  all_sites <- provenance %>%
    dplyr::select(site) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # Make a new data frame with codes replicated for each site
  all_events <- dplyr::tibble(site = rep(all_sites, each = nrow(qref)),
                       code_name = rep(hic_codes, length(all_sites)))

  # use anti_join to find which sites aren't providing certain codes
  missing_events <- dplyr::anti_join(
    all_events, unique_events,
    by = c("site" = "site",
           "code_name" = "code_name")) %>%
    dplyr::left_join(qref %>%
                dplyr::select(code_name, short_name),
              by = c("code_name" = "code_name")) %>%
    dplyr::mutate(new_name = paste0(
      str_sub(
        code_name, -4, -1), ": ", short_name))

  # make a plot highlighting missing data
  missing_events_plot <- missing_events %>%
    ggplot2::ggplot(
      ggplot2::aes(x = site, y = new_name)) +
    ggplot2::geom_tile(fill = "red", colour = "black") +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = NA)) +
    ggplot2::ylab("Code and Name of Missing Item") +
    ggplot2::ggtitle("Missing Events") +
    ggplot2::scale_x_discrete(sec.axis = dup_axis())

  ggplot2::ggsave(filename = paste0(
    path_name,
    "plots/",
    "missing_events.png"),
    plot = missing_events_plot,
    height = 40)


  # Reference Table
  reference <- make_reference(connection = ctn)

  ## Capture colour profile for consistency
  all_sites.col <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
  names(all_sites.col)  <- all_sites

  # Cases ----
  # Gives a tibble of admission numbers (patients/episodes) by week
  admissions_weekly <- reference %>%
    dplyr::distinct() %>%
    weekly_admissions()

  # Gives overall admission numbers (totals) for patients/episodes
  admissions_by_unit <-
    unit_admissions(events_table = tbls[["events"]],
                    reference_table = reference)

  for (i in seq_along(all_sites)) {
    plot_heatcal(reference_table = reference,
                 site = all_sites[i],
                 filename = paste0(
                   path_name, "plots/episodes/",
                   all_sites[i], "_admissions.png"))
  }

  # Length of Stay "Episode Length" ----

  ## This calculates the episode length for an admission
  ## This is calculated by using the discharge unit time, which is not completed
  ## universally
  ## So date and time of death is used where possible to supplement
  ## This is a complete table with a validation column, so don't use without
  ## filtering out invalid records

  # Core Table ----
  core <- make_core(ctn)

  # Epsiode_length
  episode_length <- epi_length(core_table = core,
                               reference_table = reference,
                               events_table = tbls[["events"]])

  # Seplls
  spells_user_defined <- identify_spells(
    episode_length = episode_length,
    episodes = episodes,
    minutes = spell_boundary_mins) %>%
      group_by(site) %>%
      summarise(patients = n_distinct(nhs_number),
                episodes = n_distinct(episode_id),
                spells_user = n_distinct(spell_id))

  spells_50_under <- identify_spells(
    episode_length = episode_length,
    episodes = episodes,
    minutes = spell_boundary_mins/2) %>%
      group_by(site) %>%
      summarise(spells_50_under = n_distinct(spell_id))

  spells_50_over <- identify_spells(
    episode_length = episode_length,
    episodes = episodes,
    minutes = (spell_boundary_mins+(spell_boundary_mins/2))) %>%
      group_by(site) %>%
      summarise(spells_50_over = n_distinct(spell_id))

  # This is to help give a better understanding of how sensitive our
  # decisions around
  # What defines a spell are.
  spells <- spells_user_defined %>%
    left_join(spells_50_over, by = "site") %>%
    left_join(spells_50_under, by = "site")

  # Episode validity long term average
  # typical_admissions gives me the mean and sd for the long running admissions
  # by wday

  validated_episodes <- validate_episodes(episode_length_table = episode_length,
                                          reference_table = reference,
                                          all_sites = all_sites,
                                          threshold = 10)

  # Write out this validation to the database
  validated_episodes_db <- validated_episodes %>%
    dplyr::select(episode_id, validity)

  if (DBI::dbExistsTable(ctn, "episode_validation")) {
    DBI::dbRemoveTable(ctn, "episode_validation")
  }

  DBI::dbWriteTable(
    conn =  ctn,
    name = "episode_validation",
    value = validated_episodes_db)

  print("Finished Episode Evaluation")

  ###############
  # Events =====
  ###############

  print("Starting Event Level Evaluation")

  # Set up events
  hic_event_summary <- vector(mode = "list", length = length(hic_codes))
  names(hic_event_summary) <- hic_codes

  if (DBI::dbExistsTable(ctn, "event_validation")) {
    DBI::dbRemoveTable(ctn, "event_validation")
  }

  for (i in seq_along(hic_codes)) {

    # the basics

    temp_df <- extract(core_table = core, input = hic_codes[i]) %>%
      flag_all(episode_length)

    # Plotting
    if (write_plots) {
      plot_hic(x = temp_df,
               path_name = paste0(path_name, "plots/", hic_codes[i]),
               all_sites.col = all_sites.col)
    }

    #Saving errors outside the main list
    try(hic_event_summary[[hic_codes[i]]] <- summary_main(temp_df, reference))

    hic_event_validation <- validate_event(validated_episodes, temp_df)

    if (nrow(hic_event_validation) > 0) {
    DBI::dbWriteTable(conn = ctn, name = "event_validation", value = hic_event_validation, append = TRUE)
    }

    print(paste0("finished validating: ", hic_codes[i]))

  }

  print("Finished Event Level Evaluation")

  save(hic_event_summary,
       file = paste0(path_name,
                     "data/hic_event_summary.RData"))

  save(spells,
       file = paste0(path_name,
                     "data/working_data.RData"))

  # close the connection

  DBI::dbDisconnect(ctn)

}

