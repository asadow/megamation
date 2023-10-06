#' Extract data from Megamation response
#' @param resp A Megamation API response
#' @export

mm_resp_data <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON() |>
    mm_pluck_data()
}
