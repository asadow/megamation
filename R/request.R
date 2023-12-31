#' Create a Megamation API request
#'
#' @description
#' `mm_request()` creates a request using [httr2::request()] and
#' does the following:
#'
#' * Inserts the base URL using the environment variable `MEGAMATION_URL` from
#'  your [.Renviron].
#' * Appends the URL with the endpoint defined by parameter `endpoint`.
#' * Sets the user-agent as the GitHub [megamation] package.
#' * Authenticates the request with HTTP basic authentication using
#'   environment variables `MEGAMATION_KEY` and `MEGAMATION_URL`
#'   from your `.Renviron`.
#' * Handles HTTP errors so useful information from the response is extracted
#'   (e.g. "No response body").
#' * Adds caching of responses if available. See [httr2::req_cache()].
#'
#' @param endpoint The API endpoint. For example,
#' `"timecard"` for employee transactions, and `"workorder"`
#' for work orders. All endpoints are listed at
#' https://apidocs.megamation.com/.
#' @returns An object of class `httr2_request`.
#' @keywords internal
#' @examplesIf megamation:::has_creds()
#' megamation:::mm_request("timecard")
#' megamation:::mm_request("trade")
mm_request <- function(endpoint) {
  check_string(endpoint)
  agent <- "megamation (https://github.com/asadow/megamation)"
  user <- "APIDL"
  httr2::request(mm_url()) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent(agent) |>
    httr2::req_auth_basic(user, mm_key()) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}
