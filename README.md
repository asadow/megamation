
# megamation

<!-- badges: start -->

[![R-CMD-check](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asadow/megamation/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of `megamation` is to facilitate Megamation data downloads in
R. It wraps up the [Megamation REST
API](https://apidocs.megamation.com/) so that each API endpoint (i.e. a
URL with parameters) becomes an R function with documented arguments.

`megamation` also contains and implements functions to

- Expose important details from HTTP errors in R errors.
- Provide a paginate function for large downloads.
- Caches responses to GET requests that have status code 200 and at
  least one of the standard caching headers (e.g. Expires, Etag,
  Last-Modified, Cache-Control)

## Installation

You can install the development version of `megamation` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asadow/megamation")
```

## Authorization

Your Megamation representative will provide your username, URL, and key.

``` r
username <- "APIDL"
url <- "https://api.megamation.com/uog/dl"
resource <- "timecard"
params <- list(date = I("<>09-01-2023,09-02-2023"))
api_key <- httr2::secret_decrypt(
  "4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk", 
  "MEGAMATION_KEY"
)

library(megamation)
```

## Building a custom request

`mm_req()` allows you to define a request for any resource:
`"timecard"`, `"workorder"`, etc.

``` r
req <- mm_req(url, resource, date = I("<>09-01-2023,09-29-2023"), api_key = api_key)
```

You would normally run `httr2::req_perform()`. Here I run
`httr2::req_dry_run()` to show exactly what `httr2` will send to the
server, without actually sending it.

``` r
req |> httr2::req_dry_run()
#> GET /uog/dl/timecard?DATE=<>09-01-2023,09-29-2023 HTTP/1.1
#> Host: api.megamation.com
#> User-Agent: megamation (https://github.com/asadow/megamation)
#> Accept: */*
#> Accept-Encoding: deflate, gzip
#> Authorization: <REDACTED>
```

### Performing the request

A custom request can be performed using `httr2::req_perform()`. An API
response is returned. As the body of the response is unwieldy (it
contains the data in an embedded list as raw bytes), the helper function
`mm_resp_body()` can be used to return the data.

``` r
resp <- req |> httr2::req_perform() 
resp |> mm_resp_body()
```

### Pagination

To add pagination, use `mm_req_paginate()` before the performance steps.

``` r
req |> mm_req_paginate() 
```
