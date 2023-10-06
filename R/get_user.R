get_user <- function() {
  key <- Sys.getenv("MEGAMATION_USER")
  if (!identical(key, "")) {
    return(key)
  }
  stop("No API user found, please supply with `user` argument or with MEGAMATION_USER env var")
}
