#' Create a Megamation API request
#'
#' @description
#' `mm_req()` creates a request using [httr2::request()] and
#' does the following:
#'
#' * Inserts the base URL using the environment variable `MEGAMATION_URL` from
#'  your `.Renviron`. Your key and base URL can be set using [mm_set_creds()].
#' * Appends the URL with the endpoint defined by parameter `endpoint`.
#' * Sets the user-agent as the GitHub `megamation` package.
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
#' @param .url API base URL for request.
#' @param .key API key for request.
#' @returns An object of class `httr2_request`.
#' @export
mm_req <- function(endpoint,
                   .url = get_env_url(),
                   .key = get_env_key()) {
  httr2::request(.url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent(
      "megamation (https://github.com/asadow/megamation)"
    ) |>
    httr2::req_auth_basic("APIDL", .key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}
