% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync.R
\name{sync}
\alias{sync}
\title{Synchronise Project with IDHS}
\usage{
sync(new, old)
}
\arguments{
\item{new}{the path to the new folder that contains the master active files}

\item{old}{the path to the old folder where the cleanup is directed}
}
\value{
returns nothing
}
\description{
Working within the IDHS necessitates necessitates moving between two computers
where one does not have access to git. For this reason, is it useful to
automate a syncronisation process when regularly having to move between the
two. Please use with EXTREME caution, as this function overwrites and deletes
files as specified
}
\details{
Hidden files are ignored, which is useful as we don't want to break the
.git folder anyway
}
