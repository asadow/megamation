% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{mm_get_col_info}
\alias{mm_get_col_info}
\alias{mm_get_criteria}
\alias{mm_get_schema}
\alias{mm_get_labels}
\title{Get column names, filter statuses, descriptions, and types}
\usage{
mm_get_col_info(endpoint)

mm_get_criteria(endpoint)

mm_get_schema(endpoint)

mm_get_labels(endpoint)
}
\arguments{
\item{endpoint}{The API endpoint. For example,
\code{"timecard"} for employee transactions, and \code{"workorder"}
for work orders. All endpoints are listed at
https://apidocs.megamation.com/.}
}
\value{
A data frame of class \code{\link[tibble:tbl_df-class]{tbl_df}}
containing the endpoint's appendix.
}
\description{
\code{mm_get_col_info()} returns column names, filter status, description,
and types. It does so by combining the results of the criteria and schema
appendices.

\code{mm_get_criteria()} returns column names and filter status.

\code{mm_get_schema()} returns column names, descriptions, and types.

\code{mm_get_labels()} returns column names and descriptions.
}
\examples{
\dontshow{if (megamation:::has_creds()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mm_get_col_info("status")
\dontshow{\}) # examplesIf}
}
