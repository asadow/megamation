#' Get `MEGAMATION_KEY` env var
#' @export
#' @keywords internal
get_env_key <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    cred_error("key")
  }
}

#' @export
#' @keywords internal
cred_error <- function(x) {
  cli::cli_abort(c(
    "No {.envvar MEGAMATION_{toupper(x)}} found.",
    "i" = "Did you run {.fun mm_set_creds}?"
  ))
}

#' @export
#' @keywords internal
testing_key <- function() {
  httr2::secret_decrypt("4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk", "HTTR2_KEY")
}

#' Get `MEGAMATION_URL` env var
#' @export
#' @keywords internal
get_env_url <- function() {
  url <- Sys.getenv("MEGAMATION_URL")
  if (!identical(url, "")) {
    return(url)
  }
  cred_error("url")
}

#' Get `MEGAMATION_USER` env var
#' @export
#' @keywords internal
get_env_user <- function() {
  user <- Sys.getenv("MEGAMATION_USER")
  if (!identical(user, "")) {
    return(user)
  }
  cred_error("user")
}
