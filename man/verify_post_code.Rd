% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify_codes.R
\name{verify_post_code}
\alias{verify_post_code}
\title{Verify Post Code}
\usage{
verify_post_code(post_code = NULL)
}
\arguments{
\item{post_code}{character vector of post codes}
}
\value{
a logical vector
}
\description{
Verify that a particular string conforms to a UK Post Code. Note, this does
not actually check a database (i.e. validation) to see if the post code
exists. It only checks that the Post Code meets the correct technical
specification for a postcode. There are two reasons why validation isn't a
good fit for cc-hic. First, the use of an API invariably means internet
access and sending our postcodes out of the secure system (bad). Second, the
use of the royal mail database for this purpose isn't free and would make
inspectEHR larger than is necessary.
}
\examples{
verify_post_code("AL5 3HE")
}
