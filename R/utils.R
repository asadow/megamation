#' Remove API `_links` column in data
#' @param .data Data within parsed response body.
#' @returns A data frame containing the endpoint data without a `_links` column.
#' @export
#' @keywords internal
#' @examples
#' mtcars[, "_links"] <- "some-api-url.com"
#' mtcars |> remove_api_urls() |> names()
remove_api_urls <- function(.data) {
  .data[, !names(.data) %in% "_links"]
}
