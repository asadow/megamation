#' Parse the Megamation API response
#' @param resp API response given by mm_req() or httr2::req()
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is needed due to
#' a UTF-8 issue from Megamation's side.
#' @export

mm_parse <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON()
}

#' Keep only data from the JSON list returned by Megamation's API
#' @param list_from_json A list returned by `jsonlite::fromJSON()`.
#' @export
#' @keywords internal

mm_keep_df <- function(list_from_json) {
  list_from_json |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

#' Extract data from Megamation response
#' @param resp A Megamation API response.
#' @export

mm_resp_data <- function(resp) {
  resp |>
    mm_parse() |>
    mm_keep_df() |>
    tibble::as_tibble()
}
