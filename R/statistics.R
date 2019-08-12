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
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr as_tibble mutate select pull bind_rows
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tidyr separate
#'
#' @examples
#' ks_test(heart_rates)
ks_test <- function(x) {
  sites <- x %>%
    dplyr::distinct(.data$site) %>%
    dplyr::pull()

  stopifnot(length(sites) < 2, "Less than 2 sites, so comparisons cannot be made")

  site_pairs <- utils::combn(sites, 2)

  ks_list <- base::vector(
    mode = "list",
    length = base::ncol(site_pairs)
  )

  for (i in 1:ncol(site_pairs)) {
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

  site_pairs_t %<>%
    mutate(paired_name = paste(.data$site_a, .data$site_b, sep = "-")) %>%
    select(.data$paired_name) %>%
    pull()

  names(ks_list) <- site_pairs_t

  df <- purrr::map(ks_list, .f = broom::tidy) %>%
    dplyr::bind_rows(.id = "source") %>%
    tidyr::separate(source, into = c("Site_A", "Site_B"), sep = "-")

  return(df)
}


#' Plot KS Distances
#'
#' Produces a plot of the KS distances between sites.
#' This is quite hard coded at the moment, and so could be improved in a future version
#'
#' @param x an object from \code{ks_test}
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate select bind_rows mutate_at vars funs
#' @importFrom ggplot2 aes geom_tile scale_fill_viridis_c theme_minimal coord_equal ylab xlab
#' @importFrom scales viridis_pal
ks_plot <- function(x) {
  df <- x %>%
    select(Site_A, Site_B, statistic)

  ## And now we reverse the columns so the downstream plot
  df_copy <- df
  names(df_copy) <- c("Site_B", "Site_A", "statistic")

  df <- bind_rows(df, df_copy)

  df <- bind_rows(
    df,
    tibble(
      Site_A = c("UCL", "RYJ", "RGT", "OUH", "GSTT"),
      Site_B = c("UCL", "RYJ", "RGT", "OUH", "GSTT"),
      statistic = 0
    )
  )

  out <- df %>%
    mutate_at(
      .vars = vars(Site_A, Site_B), .funs = factor,
      levels = c("UCL", "RYJ", "RGT", "OUH", "GSTT"),
      labels = c("UCL", "Imperial", "Cambridge", "Oxford", "GSTT")
    ) %>%
    ggplot(
      aes(
        x = .data$Site_A, y = .data$Site_B, fill = .data$statistic
      )
    ) +
    lims(fill = c(0, 1)) +
    geom_tile() +
    scale_fill_gradientn(colours = scales::viridis_pal()(9), limits = c(0, 1), na.value = "grey50") +
    coord_equal() +
    ylab("Comparitor Site A") +
    xlab("Comparitor Site B") +
    guides(fill = guide_legend(title = "KS Distance"))

  return(out)
}
