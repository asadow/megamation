% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull.R
\name{mm_data}
\alias{mm_data}
\title{Download data}
\usage{
mm_data(endpoint, ..., allfields = TRUE)
}
\arguments{
\item{endpoint}{The API endpoint. For example, \code{"timecard"} for employee
transactions, and \code{"workorder"} for work orders. All endpoints are listed
at \url{https://apidocs.megamation.com/}.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> Name-value pairs to filter the
request. The name should be the lower-case name of a field that is
filter-enabled (in Megamation's words, a criteria). These arguments are
processed with \code{\link[rlang:defusing-advanced]{rlang::quos()}} and support unquote via \code{\link{!!}} and
unquote-splice via \code{\link{!!!}}.}

\item{allfields}{If \code{TRUE}, return all fields currently available for the
endpoint.}
}
\value{
A data frame of class \code{\link[tibble:tbl_df-class]{tbl_df}} containing
the requested data.
}
\description{
\code{mm_data()} downloads Megamation tables into R. It automatically
\enumerate{
\item Creates and performs the necessary API GET request(s)
\item Extracts and combines the data from the API response(s)
\item Informs when no data is returned or when the API errors
}
}
\examples{
\dontshow{if (megamation:::has_creds()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
date <- seq.Date(as.Date("2023-01-01"), as.Date("2023-01-03"), by = "day")
mm_data("timecard", date = date)
\dontshow{\}) # examplesIf}
}
