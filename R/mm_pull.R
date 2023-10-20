#' Define and perform a Megamation API request
#' @param endpoint The endpoint endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param url The API URL.
#' @param key The API key.
#' @export
mm_pull <- function(endpoint,
                    ...,
                    .for = "data",
                    .url = get_env_url(),
                    .key = get_env_key()) {
  resp <- mm_req(endpoint, ..., .for = .for, .url = .url, .key = .key) |>
    mm_req_paginate() |>
    mm_req_perform_paginate()

  resp <- if(length(resp) == 1) {
    resp |>
      purrr::list_flatten() |>
      mm_keep_df() |>
      tibble::as_tibble(.name_repair = "minimal")
  }
}
