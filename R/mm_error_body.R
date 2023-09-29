#' Extract error body from a Megamation response
#' @param resp An API response.
#' @export

mm_error_body <- function(resp) {
resp |> httr2::resp_body_json() |> purrr::pluck("detail")
}
