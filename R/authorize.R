#' Install Megamation credentials
#'
#' @description
#' `mm_auth()` installs your credentials so that `megamation` functions
#' that need them, like [mm_get()], can use them automatically.
#'
#' Specifically, it places your Megamation API key and base URL
#' in your computer's `megamation` R package configuration files. Then,
#' whenever the package is loaded, your credentials are available as environment
#' variables.
#'
#' @param key Single string of the Megamation API key.
#' @param url Single string of the Megamation API base URL.
#' @returns Writes `MEGAMATION_KEY` and `MEGAMATION_URL` environment variables.
#' @export
#' @examples
#' \dontrun{
#' mm_auth(
#'   key = "<YOUR-MEGAMATION_KEY>",
#'   url = "<YOUR-MEGAMATION_URL>"
#' )
#' }
mm_auth <- function(key, url) {
  check_string(key)
  check_string(url)
  url <- check_url(url)
  .set.profile(
    MEGAMATION_KEY = key,
    MEGAMATION_URL = url,
    pkgname = "megamation"
    )
}
