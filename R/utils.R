#' Remove API `_links` column in data
#' @param .data Data within parsed response body.
#' @export
remove_api_urls <- function(.data) {
  .data[, !names(.data) %in% "_links"]
}
