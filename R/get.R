#' Get MEGAMATION_KEY env var
#' @export
#' @keywords internal

get_key <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    stop("No API key found, please supply with `key` argument or with MEGAMATION_KEY env var")
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
  httr2::secret_decrypt("4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk", "HTTR2_KEY")
}

#' Get MEGAMATION_URL env var
#' @export
#' @keywords internal

get_url <- function() {
  key <- Sys.getenv("MEGAMATION_URL")
  if (!identical(key, "")) {
    return(key)
  }
  stop("No API URL found, please supply with `url` argument or with MEGAMATION_URL env var")
}

#' Get MEGAMATION_USER env var
#' @export
#' @keywords internal

get_user <- function() {
  key <- Sys.getenv("MEGAMATION_USER")
  if (!identical(key, "")) {
    return(key)
  }
  stop("No API user found, please supply with `user` argument or with MEGAMATION_USER env var")
}
