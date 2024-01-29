#' Remove API `_links` column in data
#' @param .data Data within parsed response body.
#' @returns A data frame containing the endpoint data without a `_links` column.
#' @keywords internal
remove_api_urls <- function(.data) {
  .data[, !names(.data) %in% "_links"]
}

# Error on absent MEGAMATION env vars
stop_missing_cred <- function() {
  cli::cli_abort(c(
    "Missing credentials.",
    "i" = "Run {.fun mm_auth(key = '<your-key>', url = 'your-base-url')} to
    install your credentials."
  ))
}
