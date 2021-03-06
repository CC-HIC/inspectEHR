% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify_episodes.R
\name{verify_episodes}
\alias{verify_episodes}
\title{Verify Episodes}
\usage{
verify_episodes(episode_length = NULL, threshold = 10)
}
\arguments{
\item{episode_length}{a table created by \code{\link{characterise_episodes}}}

\item{threshold}{threshold number of days to use in calculation}
}
\value{
a table of the same form as \code{episode_length} with an additional
  column indicating if the episode comes from a valid period of time.
}
\description{
Determines which months are invalid based on a low contribution of data.
This is based upon the long term daily average for admissions. Days that
fall below 2 SD of the long term mean are flagged. If more than the
\code{threshold} number of flagged days occur in a single month, then the
month is invalidated and removed from further analysis.
}
\details{
This procedure removes episodes that occur during particularly sparse periods
(as this is likely that these months are contributing poor data) and return
only episodes that have a logical consistency in non-sparse months. The
analyst should consider if the denominator for the number of study months
should be changed following the use of this function.
}
