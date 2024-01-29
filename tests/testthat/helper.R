status_get <- function(.from) {
  mm_req("status") |>
    mm_req_append(.from) |>
    httr2::req_perform()
}
