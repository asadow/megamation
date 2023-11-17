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
    cred_error("key")
  }
}

#' Get Megamaton URL
#' @returns The string value of the `MEGAMATION_URL` environment variable
#' or an error if none exists.
#' @keywords internal
mm_url <- function() {
  url <- Sys.getenv("MEGAMATION_URL")
  if (!identical(url, "")) {
    return(url)
  }
  cred_error("url")
}

#' Get testing key
#'
#' testing_key() uses the `MEGAMATION_KEY_HTTR2` environment
#' variable to decrypt a secret.
#' @returns A string of a decrypted key.
#' @keywords internal
testing_key <- function() {
  httr2::secret_decrypt(
    "4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk",
    "MEGAMATION_KEY_HTTR2"
  )
}
