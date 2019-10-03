dist_compare <- function(x) {
  code_name <- attr(x, "code_name")
  qref %>%
    filter(code_name == code_name) %>%
    select(dist_compare) %>%
    pull()

  if (dist_compare == "xe") {
    x <- cross_entropy(x)
  } else if (dist_compare == "ks") {
    x <- ks_test(x)
  } else {
    x <- NULL
  }
  return(x)
}


#' Pairwise Kolmogorov-Smirnov Distance
#'
#' Takes a distribution from a CC-HIC dataitem and returns
#' the pairwise KS distance by site.
#'
#' @param x extracted data item from \code{\link{extract}}
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom utils combn
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble mutate select pull bind_rows
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tidyr separate
ks_test <- function(x) {

  code_name <- attr(x, "code_name")
  sites <- unique(x$site)
  site_count <- length(sites)
  if (site_count < 2) {
    rlang::inform("Comparison must have 2 or more sites")
    return(FALSE)
  }
  site_pairs <- utils::combn(sites, 2)

  if (!(class(x$value)[1] %in% c("numeric", "integer"))) {
    x$value <- as.numeric(x$value)
  }

  ks_list <- base::vector(
    mode = "list",
    length = base::ncol(site_pairs)
  )

  for (i in seq_along(ks_list)) {
    ks_list[[i]] <- ks.test(
      x = x %>%
        filter(.data$site == site_pairs[, i][1]) %>%
        select(.data$value) %>%
        pull(),
      y = x %>%
        filter(.data$site == site_pairs[, i][2]) %>%
        select(.data$value) %>%
        pull()
    )
  }

  site_pairs_t <- t(site_pairs) %>%
    as_tibble()

  names(site_pairs_t) <- c("site_a", "site_b")

  site_pairs_t <- site_pairs_t %>%
    mutate(paired_name = paste(.data$site_a, .data$site_b, sep = "-")) %>%
    select(.data$paired_name) %>%
    pull()

  names(ks_list) <- site_pairs_t

  df <- map(ks_list, .f = broom::tidy) %>%
    bind_rows(.id = "source") %>%
    separate(source, into = c("Site_A", "Site_B"), sep = "-")

  attr(df, "code_name") <- code_name
  return(df)
}


#' Calculate Cross Entropy
#'
#' IN DEVELOPMENT
#'
#' Calculates the cross entropy of two vectors: \code{u} and \code{v}. This is
#' useful is decribing how different two distributions (particularly if
#' categorical) are.
#'
#' @param x an extracted data item
#'
#' @importFrom rlang abort
#'
#' @return the cross entropy of \code{u} and \code{v}
#' @export
cross_entropy <- function(x){

  code_name <- attr(x, "code_name")
  sites <- unique(x$site)
  site_count <- length(sites)
  if (site_count < 2) rlang::abort("Comparison must have 2 or more sites")
  site_pairs <- utils::combn(sites, 2)

  xe_list <- base::vector(
    mode = "list",
    length = base::ncol(site_pairs)
  )

  for (i in seq_along(site_pairs)) {
    xe_list[[i]] <- xe(
      u = x %>%
        filter(.data$site == site_pairs[, i][1]) %>%
        select(.data$value) %>%
        pull(),
      v = x %>%
        filter(.data$site == site_pairs[, i][2]) %>%
        select(.data$value) %>%
        pull()
    )
  }

  site_pairs_t <- t(site_pairs) %>%
    as_tibble()

  names(site_pairs_t) <- c("site_a", "site_b")

  site_pairs_t <- site_pairs_t %>%
    mutate(paired_name = paste(.data$site_a, .data$site_b, sep = "-")) %>%
    select(.data$paired_name) %>%
    pull()

  names(xe_list) <- site_pairs_t

  # df <- map(ks_list, .f = broom::tidy) %>%
  #   bind_rows(.id = "source") %>%
  #   separate(source, into = c("Site_A", "Site_B"), sep = "-")
  #
  # attr(df, "code_name") <- code_name
  # return(df)

}


xe <- function(u, v) {
  if (length(u) != length(v)) abort("`u` and `v` must be of the same length")
  x <- 0
  for (i in seq_len(u)){
    x <- x + (u[i] * log(v[i]))
  }
  return(-x)
}
