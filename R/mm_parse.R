#' Parse the Megamation API response
#' @param resp API response given by mm_req() or httr2::req()
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is needed due to
#' a UTF-8 issue from Megamation's side.
#' @export
#' @keywords internal

mm_parse <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON()
}
