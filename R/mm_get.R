#' Perform a GET request to Megamation's API
#' @param endpoint The endpoint endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param .give What the response should give:
#' `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .url The API URL.
#' @param .key The API key.
#' @export

mm_get <- function(endpoint,
                   ...,
                   .give = "data",
                   .url = get_url(),
                   .key = get_key(),
                   .full_resp = C(FALSE, TRUE)) {

  req <- mm_req(endpoint, ..., .give = .give, .url = .url, .key = .key)

  paginatable <- .give == "data"

# These three steps all depend on paginatable -----------------------------

# Treat as class and use method instead? -----------------------------------------------------

  req <- if(!paginatable) {
    req
  } else{
    req |> mm_req_paginate()
  }

  resp <- req |> mm_req_perform()
  mm_resp_process(resp)

}
