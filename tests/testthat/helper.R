status_get <- function(.from) {
  .from <- "schema"

  resp <- mm_req("status") |>
    mm_req_append(.from) |>
    httr2::req_perform()
}
