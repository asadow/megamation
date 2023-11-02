status_get <- function(.from) {
  resp <- mm_req("status") |>
    mm_req_append(.from) |>
    httr2::req_perform()
}
