#' Extract error body from a Megamation API response
#'
#' `mm_error_body()` simply plucks the `detail` list from the response body,
#' where Megamation's API includes informative error messages.
#'
#' @param resp An API response.
#' @returns A string from the `detail` index of a named
#' list (the parsed response).
#' @keywords internal
#' @examples
#' fake_mm_resp <- httr2::response_json(
#'   body = list(
#'     detail = "This is a fake detail/message from the API's response."
#'   )
#' )
#' megamation:::mm_error_body(fake_mm_resp)
mm_error_body <- function(resp) {
  if (!httr2::resp_has_body(resp)) {
    return("No response body.")
  }
  httr2::resp_body_json(resp) |> purrr::pluck("detail")
}
