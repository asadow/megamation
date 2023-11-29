#' Get Megamation key
#' @returns The string value of the `MEGAMATION_KEY` environment variable
#' or an error if none exists.
#' @keywords internal
mm_key <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (testthat::is_testing()) {
    return(testing_key())
  } else {
    stop_missing_cred()
  }
}

#' Get Megamaton URL
#' @returns The string value of the `MEGAMATION_URL` environment variable
#' or an error if none exists.
#' @keywords internal
mm_url <- function() {
  url <- Sys.getenv("MEGAMATION_URL")
  base_url <- "https://api.megamation.com/"

  if (identical(url, "")) {
    stop_missing_cred()
  }

  if (!startsWith(url, base_url)) {
    cli::cli_abort(
      "{.envvar MEGAMATION_URL} must be of the form
      {.val {base_url}<institution ID>/dl}, not {.val url}."
    )
  }

  if (endsWith(url, "/")) sub("/$", "", url) else url

}

#' Get testing key
#'
#' testing_key() uses the `MEGAMATION_KEY_HTTR2` environment
#' variable to decrypt a secret.
#' @returns A string of a decrypted key.
#' @keywords internal
testing_key <- function() {
  httr2::secret_decrypt(
    "A0RezbBRC8_7F2uGNHujFBDfSAy5hukOeWU",
    "MEGAMATION_KEY_HTTR2"
  )
}
