#' #' Verify Patients based upon Study Requirements
#' #'
#' #' Interrogates the CC-HIC database and provides a vector of episode ids that
#' #' can "safely" be used for your study based on the data you require. This
#' #' means that these episodes meet the following criteria:
#' #' - Episodes themselves are valid (see verify_episodes)
#' #' - Episodes come from sites that contribute data for the chosen fields
#' #' - Episodes come from time period where we know data is being contributed.
#' #'
#' #' @importFrom tidyr expand_grid
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' verify_study <- function(
#'   code_names, from, to,
#'   database = NULL,
#'   username = NULL,
#'   password = NULL,
#'   host = "localhost",
#'   port = 5432,
#'   sqlite_file = NULL) {
#'   
#'   stopifnot(
#'     all(!is.null(database), !is.null(username), !is.null(password)) ||
#'       !is.null(sqlite_file)
#'   )
#'   
#'   ## To add... query database first and see if validation tables are present
#'   ## Ask if you would like to use these tables instead of running fresh
#'   
#'   inform("Starting episode evaluation")
#'   
#'   if (!all(code_names %in% qref$code_names)) {
#'     rlang::abort("You are trying to evaluate for data items not contained in HIC")
#'   }
#'   
#'   hic_codes <- code_names
#'     
#'   # Prepare Connections
#'   ctn <- connect(
#'     database = database,
#'     username = username,
#'     password = password,
#'     host = host,
#'     port = port,
#'     sqlite_file = sqlite_file
#'   )
#'     
#'   tbls <- retrieve_tables(ctn)
#'     
#'   # Collect small tables
#'   episodes <- collect(tbls[["episodes"]])
#'   provenance <- collect(tbls[["provenance"]])
#'   metadata <- collect(tbls[["variables"]])
#'     
#'   ## Missing fields
#'   # we want to identify fields that are not contributed by a site
#'   # so we must retrive the unique events that each site contributes first
#'   unique_events <- tbls[["events"]] %>%
#'     left_join(tbls[["episodes"]], by = "episode_id") %>%
#'     left_join(tbls[["provenance"]], by = c("provenance" = "file_id")) %>%
#'     select(site, code_name) %>%
#'     distinct() %>%
#'     collect() %>%
#'     mutate(contributed = "Yes")
#'     
#'   # Capture all the sites currently contributing to the project
#'   all_sites <- provenance %>%
#'     dplyr::select(site) %>%
#'     dplyr::distinct() %>%
#'     dplyr::pull()
#'     
#'   # Make a new data frame with codes replicated for each site
#'   all_events <- expand_grid(
#'     site = all_sites,
#'     code_name = hic_codes)
#'     
#'   # use anti_join to find which sites aren't providing certain codes
#'   missing_events <- full_join(
#'     all_events, unique_events, by = c("site", "code_name")) %>%
#'     mutate(contributed = if_else(is.na(contributed), "No", contributed)) %>%
#'     filter(!is.na(site))
#'     
#'   # make a plot highlighting missing data
#'     missing_events_plot <- missing_events %>%
#'       ggplot(aes(x = site, y = new_name, fill = contributed)) +
#'       geom_point(shape = 21, size = 4.5) +
#'       theme_minimal() +
#'       theme(
#'         panel.grid.major.x = element_blank(),
#'         panel.grid.minor.x = element_blank(),
#'         panel.background = element_blank()
#'       ) +
#'       ylab("Code and Name of Data Item") +
#'       xlab("Site")
#'     
#'     ggsave(
#'       filename = paste0(
#'         output_folder,
#'         "plots/",
#'         "missing_events.svg"
#'       ),
#'       plot = missing_events_plot,
#'       height = 60, width = 5, limitsize = FALSE
#'     )
#'     
#'     # Useful Tables
#'     core <- make_core(connection = ctn)
#'     reference <- make_reference(connection = ctn)
#'     
#'     ## Capture colour profile for consistency
#'     all_sites.col <- viridis_pal()(length(all_sites))
#'     names(all_sites.col) <- all_sites
#'     
#'     # Cases ----
#'     # Gives a tibble of admission numbers (patients/episodes) by week
#'     admissions_weekly <- reference %>%
#'       dplyr::distinct() %>%
#'       weekly_admissions()
#'     
#'     # Gives overall admission numbers (totals) for patients/episodes
#'     admissions_by_unit <-
#'       unit_admissions(
#'         events_table = tbls[["events"]],
#'         reference_table = reference
#'       )
#'     
#'     for (i in seq_along(all_sites)) {
#'       temp_heat <- make_heatcal(reference, site = all_sites[i])
#'       temp_plot <- plot(temp_heat, display = FALSE)
#'       ggsave(
#'         temp_plot,
#'         filename = paste0(
#'           output_folder, "plots/episodes/", all_sites[i], "_admissions.svg"
#'         ), height = 8, width = 12.88
#'       )
#'     }
#'     
#'     # Length of Stay "Episode Length" ----
#'     # Epsiode_length
#'     episode_length <- characterise_episodes(ctn)
#'     spell_length <- characterise_spells(episode_length)
#'     
#'     # Episode validity long term average
#'     ve_episodes <- verify_episodes(episode_length = episode_length)
#'     
#'     # Write out this validation to the database
#'     episode_verification <- ve_episodes %>%
#'       select(episode_id, veracity)
#'     
#'     copy_to(ctn, episode_verification, temporary = FALSE, overwrite = TRUE)
#'     
#'     rlang::inform("Finished episode evaluation")
#'     rlang::inform("Starting event evaluation")
#'     
#'     # Set up events
#'     hic_event_summary <- vector(mode = "list", length = length(hic_codes))
#'     names(hic_event_summary) <- hic_codes
#'     
#'     for (i in seq_along(hic_codes)) {
#'       
#'       # Extract the event in question from the DB
#'       df <- extract(core_table = core, input = hic_codes[i])
#'       
#'       # Check to see if there is any data there
#'       if (nrow(df) == 0 || sum(is.na(df$value)) == nrow(df)) {
#'         rlang::inform(glue("Skipping over {hic_codes[i]} as no data present"))
#'       } else {
#'         
#'         # Basic Verification: range, boundary, duplcation, periodicity
#'         df <- verify_events(df, ve_episodes)
#'         
#'         # Coverage verification
#'         df_cov <- coverage(df, reference_tbl = reference)
#'         
#'         # Statistical verification
#'         ks_pos <- qref[qref$code_name == hic_codes[i], "dist_compare", drop = TRUE]
#'         if (!is.na(ks_pos) && ks_pos == "ks") {
#'           df_stat <- ks_test(df)
#'         } else {
#'           df_stat <- NA
#'         }
#'         
#'         # Saving errors outside the main list
#'         hic_event_summary[[i]] <- summarise_verification(df, df_stat, df_cov, reference)
#'         
#'         # The plotting
#'         if (write_plots) {
#'           # Skip over these for the time being
#'           # They are confidential, so we don't want to plot them out right
#'           # Now. But we do need a better way to represent these items
#'           
#'           for (s in seq_along(all_sites)) {
#'             hc <- make_heatcal(
#'               reference_tbl = reference, dataitem_tbl = df, site = all_sites[s])
#'             
#'             if (hc != FALSE) {
#'               hc_plot <- plot(hc, display = FALSE)
#'               ggsave(hc_plot, filename = glue(
#'                 "{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_heatcal_{all_sites[s]}.svg"
#'               ))
#'             }
#'             
#'           }
#'           
#'           if (!is.na(ks_pos) && ks_pos == "ks") {
#'             if (df_stat != FALSE) {
#'               stat_plot <- plot_ks(df_stat, reference)
#'               ggsave(stat_plot, filename = glue(
#'                 "{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_ks.svg"
#'               ))
#'             }
#'           }
#'           
#'           if (!(hic_codes[i] %in% paste0(
#'             "NIHR_HIC_ICU_0", c(
#'               "001", "002", "003", "004",
#'               "005", "073", "076", "399",
#'               "088", "912")))) {
#'             plots <- plot(x = df, display = FALSE)
#'             purrr::imap(plots,
#'                         ~ ggsave(.x,
#'                                  filename = glue(
#'                                    "{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_{.y}.svg"
#'                                  )
#'                         )
#'             )
#'           }
#'         }
#'         rlang::inform(glue("Finished validating: {hic_codes[i]}"))
#'       }
#'     }
#'     save(hic_event_summary,
#'          file = paste0(
#'            output_folder,
#'            "data/hic_event_summary.RData"
#'          )
#'     )
#'     rlang::inform("Finished event level evaluation")
#'     # close the connection
#'     DBI::dbDisconnect(ctn)
#'     return(TRUE)
#'   }