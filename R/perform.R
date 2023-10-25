
#' Perform a GET request to Megamation's API
#'
#' @description
#' `mm_get()` accomplishes the full process of a GET request:
#'
#' * Creates an API request and defines its behaviour.
#' * Performs the request and fetches the response.
#' * Converts the body of the response to a data frame.
#'
#' Where applicable, pagination is automatically applied to the request
#' by [mm_req_paginate()] and returned pages are automatically combined.
#'
#' @inheritParams mm_request
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested information.
#' @export
mm_get <- function(endpoint,
                   ...,
                   .get = "data",
                   .paginate = TRUE,
                   .url = get_env_url(),
                   .key = get_env_key()) {
  .get <- rlang::arg_match(.get, c("data", "criteria", "labels", "schema"))

  req <- mm_request(
    endpoint,
    ...,
    .get = .get,
    .paginate = .paginate,
    .url = .url,
    .key = .key
  )
  resp <- mm_req_perform(req)

  tbl_result <- if (!is_paginated(req)) {
    resp[[1]] |>
      resp_body_parse() |>
      parsed_to_tbl(.get)
  } else {
    resp |>
      purrr::map(
        \(x) x |>
          resp_body_parse() |>
          parsed_keep_df()
      ) |>
      mm_bind_then_tbl()
  }

  remove_api_urls(tbl_result)

}

#' Perform a Megamation API request
#'
#' @description
#'
#' After creating a request with [mm_request()],
#' call `mm_req_perform()` to perform it and fetch
#' the results back to R.
#'
#' Where the request is paginated, pagination is automatically performed
#' by [httr2::req_perform_iteratively()].
#'
#' @inheritParams mm_req_paginate
#' @returns A list of HTTP responses. Each response is
#' an S3 list with class `httr2_response`. (For more on this class,
#' see [httr2::response].) If the request was paginated, these
#' responses correspond to pages.
#' @export
mm_req_perform <- function(req) {
  check_request(req)

  if (!is_paginated(req)) {
    req |>
      httr2::req_perform() |>
      list()
  } else {
    req |>
      httr2::req_perform_iteratively()
  }
}