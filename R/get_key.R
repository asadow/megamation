get_key <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    stop("No API key found, please supply with `api_key` argument or with MEGAMATION_KEY env var")
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
  httr2::secret_decrypt("4E5GlxeUybPJnCQQnwyDGsPIncZI526gyfk", "HTTR2_KEY")
}
