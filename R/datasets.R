#' Data quality reference table for CC-HIC. Version 1.1
#'
#' A dataset containing useful metadata of each dataitem in CC-HIC
#'
#' @format A \code{tibble} with 255 rows and 11 variables:
#' \describe{
#'   \item{code_name}{the NIHR_HIC_ICU number}
#'   \item{short_name}{a short name}
#'   \item{long_name}{a longer name}
#'   \item{primary_column}{the primary column used for data storage in the events table of the cc-hic database}
#'   \item{type}{1d/2d}
#'   \item{class}{hic data class}
#'   \item{range_min}{a lower boundary for a normal physiological value}
#'   \item{range_max}{an upper boundary for a normal physiological value}
#'   \item{ref_lower}{a lower boundary for a possible value}
#'   \item{ref_upper}{an upper boundary for a possible value}
#'   \item{assumed_units}{the assumed units}
#'   \item{notes}{any other notes}
#'   \item{possible_values}{a list column with possible values the dataitem can take}
#' }
"qref"
