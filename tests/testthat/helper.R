status_get <- function(.from) {
  mm_request("status") |>
    mm_req_append(.from) |>
    httr2::req_perform()
}
