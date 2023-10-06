get_url <- function() {
  key <- Sys.getenv("MEGAMATION_URL")
  if (!identical(key, "")) {
    return(key)
  }
  stop("No API URL found, please supply with `url` argument or with MEGAMATION_URL env var")
}
