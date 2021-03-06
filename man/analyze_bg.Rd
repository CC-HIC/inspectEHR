% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_bg.R
\name{analyze_bg}
\alias{analyze_bg}
\title{Analyze Blood Gases}
\usage{
analyze_bg(pxo2, pxco2, ph, probability = FALSE)
}
\arguments{
\item{pxo2}{numeric vector of oxygen partial pressures}

\item{pxco2}{numeric vector of co2 partial pressures}

\item{ph}{numeric vector of pH}

\item{probability}{logical flag. If true return the probability that the sample came from an Arterial line.}
}
\value{
a character vector of either "arterial" or "venous".
}
\description{
labells blood sampels according to whether or not they came from an arterial or venous source.
}
