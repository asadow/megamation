#' Get `MEGAMATION_KEY` env var
#' @returns The string value of the MEGAMATION_KEY environment variable
#' or an error if none exists.
#' @export
#' @keywords internal
get_env_key <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (testthat::is_testing()) {
    return(testing_key())
  } else {
    cred_error("key")
  }
}

#' Get personal key
#'
#' testing_key() uses the HTTR2_KEY_MEGAMATION environment
#' variable to decrypt a secret.
#' @returns A string of a decrypted key.
#' @export
#' @keywords internal
testing_key <- function() {
  httr2::secret_decrypt(
    "4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk",
    "HTTR2_KEY_MEGAMATION"
  )
}

#' Get `MEGAMATION_URL` env var
#' @returns The string value of the MEGAMATION_URL environment variable
#' or an error if none exists.
#' @export
#' @keywords internal
get_env_url <- function() {
  url <- Sys.getenv("MEGAMATION_URL")
  if (!identical(url, "")) {
    return(url)
  }
  cred_error("url")
}
