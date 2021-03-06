% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify_events.R
\name{verify_bounds_2d}
\alias{verify_bounds_2d}
\title{verify boundaries of 2d data items}
\usage{
verify_bounds_2d(x = NULL, los_table = NULL, hours = 24)
}
\arguments{
\item{x}{an extracted nhic event table}

\item{los_table}{episode length table}

\item{hours}{the number of hours you allow before and after an episode}
}
\value{
a two column tibble with event id and boundary status
}
\description{
verify boundaries of 2d data items
}
