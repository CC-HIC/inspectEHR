% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/characterise_episodes.R
\name{resolve_date_time}
\alias{resolve_date_time}
\title{Resolve DateTime}
\usage{
resolve_date_time(df = NULL, date_code = NULL, time_code = NULL)
}
\arguments{
\item{df}{a table that contains columns for the date and time of interest}

\item{date_code}{the column name for the date of interest}

\item{time_code}{the column name for the time of interest}
}
\value{
a table with the correct datetime pairing for the codes given
}
\description{
Many events in CC-HIC are stored in separate date and time columns/objects.
This function attempts to reconcile and combine these times when possible. Of
note, date and time information is not always stored with consistent rules.
For example, death date and time, are often stored for every patient in every
episode, even though the patient can only die once. The following are some
date and time pairings that denote a singular event:
\itemize{
  \item "NIHR_HIC_ICU_0042", "NIHR_HIC_ICU_0043" - Unit Death
  \item "NIHR_HIC_ICU_0038", "NIHR_HIC_ICU_0039" - Body Removal
  \item "NIHR_HIC_ICU_0044", "NIHR_HIC_ICU_0045" - Brain stem death
  \item "NIHR_HIC_ICU_0048", "NIHR_HIC_ICU_0049" - Treatment Withdrawal
  \item "NIHR_HIC_ICU_0050", "NIHR_HIC_ICU_0051" - Discharge ready
}
If a date or time component is missing, nothing is returned as the datetime
cannot be accurately formed.
}
