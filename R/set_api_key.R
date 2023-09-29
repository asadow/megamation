#' Set MEGATION_KEY as an environment variable
#' @param key The API key.
#' @export

set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("MEGAMATION_KEY" = key)
}
