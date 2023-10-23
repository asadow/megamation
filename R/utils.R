#' Remove API `_links` column in data
#' @export
remove_api_urls <- function(.data) {
  .data[, !names(.data) %in% "_links"]
}
