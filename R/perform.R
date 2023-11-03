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
#' @examples
#' \dontrun{
#' mm_request("status") |> mm_req_perform()
#' }
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
