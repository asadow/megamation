% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request.R
\name{mm_req_append}
\alias{mm_req_append}
\title{Append a GET request}
\usage{
mm_req_append(req, appendix)
}
\arguments{
\item{req}{An API request generated by \code{\link[=mm_req]{mm_req()}}.}

\item{appendix}{\code{"criteria"}, \code{"labels"}, or \code{"schema"}.}
}
\value{
An object of class \code{httr2_request}.
}
\description{
Append a GET request
}
\keyword{internal}
