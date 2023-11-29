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
    "i" = "Run {.fun usethis::edit_r_environ} to open your `.Renviron` file.
      Edit in these lines and restart R to apply your credentials to all
      sessions.",
    ">" = "`MEGAMATION_KEY = <your-key>`",
    ">" = "`MEGAMATION_URL = <your-URL>`",
    "i" = "`Sys.setenv()` is another option that applies only to the current
      session."
  ))
}
