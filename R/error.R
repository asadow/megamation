#' Extract error body from a Megamation API response
#' @param resp An API response.
#' @returns A string from indexing the parsed response's `detail` list name.
#' @export
mm_error_body <- function(resp) {
  if (!httr2::resp_has_body(resp)) {
    return("No response body.")
  }
  httr2::resp_body_json(resp) |> purrr::pluck("detail")
}