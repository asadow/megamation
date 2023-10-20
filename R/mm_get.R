#' Perform a GET request to Megamation's API
#' @param endpoint The endpoint endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param .for What the request is for/response should give:
#' `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .url The API URL.
#' @param .key The API key.
#' @export
mm_get <- function(endpoint,
                   ...,
                   .for = "data",
                   .url = get_env_url(),
                   .key = get_env_key(),
                   .full_resp = C(FALSE, TRUE)) {
  .for <- rlang::arg_match(.for, c("criteria", "labels", "schema", "data"))

  req <- mm_req(endpoint, ..., .for = .for, .url = .url, .key = .key)

  paginatable <- .for == "data"

  result <- if(!paginatable) {
    resp <- req |>
      mm_req_perform()
    resp |>
      resp_body_parse() |>
      parsed_to_tbl(.for)
  } else{
    list_of_resp <- req |>
      mm_req_paginate() |>
      mm_req_perform()
    list_of_resp |>
      map(
        \(x) x |>
          resp_body_parse() |>
          mm_keep_df(x)
      ) |>
      mm_bind_then_tbl()
  }

  return(result)

}
