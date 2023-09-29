#' Perform a Megamation API workorder request
#' @param url A URL starting with "https://api.megamation.com/".
#' @param status Work order status. For example, S.
#' @param type Work order type.
#' @param date Work order date.
#' @export

req_mm_wo <- function(url, status = NULL, type = NULL, date) {
  if (!is.null(status)) {
    status <- match.arg(status, c("S", "???"))
  }

  json <- mm_req(
    # url,
    "workorder",
    # status = status,
    type = type
  ) |>
    httr2::resp_body_raw() |>
    rawToChar()

  json_list <- jsonlite::fromJSON(json)

  json_list |>
    find_data()
}
