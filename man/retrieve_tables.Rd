% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{retrieve_tables}
\alias{retrieve_tables}
\title{Retrieve Database Tables}
\usage{
retrieve_tables(connection)
}
\arguments{
\item{connection}{an sql connection}
}
\value{
a list containing pointers to tables within the sql connection.
}
\description{
Places all tables from the database connection in a list. This makes use of the tables
in dplyr extremely easy.
}
\examples{
db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
ctn <- connect(sqlite_file = db_pth)
core <- make_core(ctn)
tbls <- retrieve_tables(ctn)
tbls[["events"]] # the events table
DBI::dbDisconnect(ctn)
}
