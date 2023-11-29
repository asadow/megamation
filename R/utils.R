#' Remove API `_links` column in data
#' @param .data Data within parsed response body.
#' @returns A data frame containing the endpoint data without a `_links` column.
#' @keywords internal
remove_api_urls <- function(.data) {
  .data[, !names(.data) %in% "_links"]
}

# Error on absent MEGAMATION env vars
cred_error <- function(x) {
  cli::cli_abort(c(
    "No {.envvar MEGAMATION_{toupper(x)}} found."
  ))
}
