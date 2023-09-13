#' @export

req_mm_wo <- function(url, status = NULL, type = NULL, date) {
  if (!is.null(status)) {
    status <- match.arg(status, c("S", "???"))
  }

  json <- req_mm(
    url,
    "workorder",
    status = status,
    type = type
  )

  json_list <- jsonlite::fromJSON(json)

  json_list |>
    find_data_frame()
}
