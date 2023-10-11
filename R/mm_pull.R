#' Define and perform a Megamation API request
#' @param resource The resource endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param url The API URL.
#' @param key The API key.
#' @export

mm_pull <- function(resource,
                    ...,
                    url = get_url(),
                    key = get_key()) {
  mm_req(resource, ..., url = url, key = key) |>
    mm_req_paginate() |>
    mm_req_perform_paginate() |>
    tibble::as_tibble()
}
