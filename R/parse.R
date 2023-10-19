#' Parse body from Megamation API response
#' @param resp_body A Megamation API response body.
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#' @export

mm_parse <- function(resp_body) {
  resp_body |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON()
}

#' Extract and parse body from Megamation API response
#' @param resp A Megamation API response.
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#' @export
mm_resp_body <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    mm_parse()
}

#' Keep only data from the JSON list returned by Megamation's API
#' @param list_from_json A list returned by [jsonlite::fromJSON()].
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

mm_resp_df <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    mm_parse() |>
    mm_keep_df()
}
