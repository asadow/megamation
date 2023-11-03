#' Install Megamation credentials in your `.Renviron` file for repeated use
#'
#' @description
#' This function adds your Megamation API key and base URL to your
#' [.Renviron] file so it can be called securely without being stored in
#' your code. After you have installed these two credentials, [mm_request()] and
#' [mm_get()] will call them automatically. They can be
#' called manually at any time with `Sys.getenv("MEGAMATION_KEY")` or
#' `Sys.getenv("MEGAMATION_URL")`.
#'
#' If you do not have a
#' `.Renviron` file, the function will create one for you. If you already
#' have a `.Renviron` file, the function will append the key to your
#' existing file, while making a backup of your original file for disaster
#' recovery purposes.
#'
#' @param key The API key provided to you by Megamation formatted in quotes.
#' @param url The API base URL provided to you by Megamation
#'   formatted in quotes.
#' @param overwrite If TRUE, will overwrite existing Megamation
#'   credentials that you already have in your `.Renviron` file.
#' @returns Writes `MEGAMATION_KEY` and `MEGAMATION_URL` environment variables
#' to your `.Renviron` file.
#' @export
#' @examples
#' \dontrun{
#' mm_set_creds(
#'   key = "<YOUR-MEGAMATION_KEY>",
#'   url = "<YOUR-MEGAMATION_URL>"
#' )
#' }
mm_set_creds <- function(key, url, overwrite = FALSE) {
  check_string(key)
  check_bool(overwrite)
  url <- check_url(url)

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  # If needed, backup original .Renviron before doing anything
  if (file.exists(renv)) {
    file.copy(renv, file.path(home, ".Renviron_backup"))
  }

  if (!file.exists(renv)) {
    file.create(renv)
  } else {
    if (isTRUE(overwrite)) {
      cli::cli_alert_info(
        "If needed, your original {.file {'.Renviron'}}
          is backed up and stored in
          {.file {home}}."
      )
      oldenv <- readLines(renv)
      newenv <- oldenv[-grep("MEGAMATION_KEY|MEGAMATION_URL", oldenv)]
      writeLines(newenv, renv)
    } else {
      tv <- readLines(renv)
      if (any(grepl("MEGAMATION_KEY|MEGAMATION_URL", tv))) {
        cli::cli_abort(c(
          "Megamation credentials already exist.",
          "i" = "You can set {.arg overwrite = TRUE}
              to overwrite them."
        ))
      }
    }
  }

  keyconcat <- glue::glue("MEGAMATION_KEY = '{key}'")
  urlconcat <- glue::glue("MEGAMATION_URL = '{url}'")

  # Append credentials to .Renviron file
  write(keyconcat, renv, sep = "\n", append = TRUE)
  write(urlconcat, renv, sep = "\n", append = TRUE)

  Sys.setenv(
    MEGAMATION_KEY = key,
    MEGAMATION_URL = url
  )

  cli::cli_alert_success("Set and loaded Megamation API credentials.")
}
