#' Define and perform a Megamation API request
#' @param resource The resource endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param user The API key user.
#' @param api_key The API key.
#' @export

mm_pull <- function(resource,
                    ...,
                    url = get_url(),
                    user = get_user(),
                    key = get_key()) {
  mm_req(resource, url = url, user = user, key = key) |>
    mm_req_paginate() |>
    httr2::paginate_req_perform() |>
    mm_resp_data()
}
