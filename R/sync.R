#' Synchronise Project with IDHS
#'
#' Working within the IDHS necessitates necessitates moving between two computers
#' where one does not have access to git. For this reason, is it useful to
#' automate a syncronisation process when regularly having to move between the
#' two. Please use with EXTREME caution, as this function overwrites and deletes
#' files as specified
#'
#' Hidden files are ignored, which is useful as we don't want to break the
#' .git folder anyway
#'
#' @param new the path to the new folder that contains the master active files
#' @param old the path to the old folder where the cleanup is directed
#'
#' @return returns nothing
#' @export
sync <- function(new, old) {

  stopifnot(!is.null(new), !is.null(old))

  arrivals <- list.files(new, recursive = TRUE)
  existing <- list.files(old, recursive = TRUE)

  ## We need to take 3 actions:
  # 1) overwrite any files that have changed
  # 2) delete any files that have been deleted
  # 3) copy accross any new files

  overwrites <- 0L
  additions <- 0L
  deletes <- 0L

  # go through every file in the arrivals area
  for (i in seq_along(arrivals)) {
    new_file <- paste0(new, arrivals[i])
    # check if arrival file already exists
    if (arrivals[i] %in% existing) {
      old_file <- paste0(old, arrivals[i])
      # check if the files differ. We can use Hashes for this basic functionality
      if (as.character(tools::md5sum(new_file)) != as.character(tools::md5sum(old_file))) {
        rlang::inform(glue::glue("changes detected: overwriting {new_file}"))
        invisible(file.copy(from = new_file, to = old_file, overwrite = TRUE))
        overwrites <- overwrites + 1L
      }
      # Otherwise if file is not in existing (i.e. it is a new file) copy straight across
    } else {
      rlang::inform(glue::glue("new file: copying {new_file}"))
      invisible(file.copy(from = new_file, to = paste0(old, arrivals[i])))
      additions <- additions + 1L
    }
  }

  # Now clean up any files that have been deleted
  for (j in seq_along(existing)) {
    existing_file <- paste0(old, existing[j])
    if (!(existing[j] %in% arrivals)) {
      rlang::inform(glue::glue("file removed: deleting {existing_file}"))
      invisible(file.remove(existing_file))
      deletes <- deletes + 1L
    }
  }

  rlang::inform(glue::glue(
    "summary
    additions: {additions}
    deletions: {deletes}
    overwrites: {overwrites}"))

}
