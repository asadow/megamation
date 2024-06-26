#' Perform a Megamation API request
#'
#' @description
#'
#' After creating a request with [mm_req()], call `mm_req_perform()` to perform
#' it with pagination and fetch the results back to R.
#'
#' @inheritParams mm_next_req
#' @inheritParams httr2::req_perform_iterative
#' @returns A list of HTTP responses. Each response is an S3 list with class
#'   `httr2_response`. (For more on this class, see [httr2::response].) If the
#'   request was paginated, the responses correspond to pages.
#' @keywords internal
mm_req_perform <- function(req, max_reqs = Inf) {
  check_request(req)
  httr2::req_perform_iterative(
    req,
    next_req = mm_next_req,
    on_error = "return",
    max_reqs = max_reqs
    )
}
