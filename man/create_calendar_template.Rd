% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar_heat.R
\name{create_calendar_template}
\alias{create_calendar_template}
\title{Calendar Template for Count Data}
\usage{
create_calendar_template(x = NULL, date_boundaries = NULL)
}
\arguments{
\item{x}{count data to be used in the calendar from
\code{\link{daily_admissions}}
\describe{
  \item{date}{date column formatted to a POSIX date}
  \item{count_column}{counting column of integer type}
}}

\item{date_boundaries}{character vector of format YYYY-MM-DD (or something
that is parsable by \code{lubridate::ymd}) defining the start and end
dates of the calendar. If nothing is supplied, this is derrived from the
data}
}
\value{
a tibble with correct week alignments for a calendar heatmap
}
\description{
Creates the basic template for a heatmap calendar in ggplot when using count
data. The calendar will automatically expand the date contrains to full
years. Can be supplied the output from \code{\link{daily_admissions}}.
}
