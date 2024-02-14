#' Create a Megamation API request
#'
#' @description
#' `mm_req()` creates a request using [httr2::request()] and
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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs to filter the
#' request. The name should be the lower-case name of a field that is
#' filter-enabled (in Megamation's words, a criteria). These arguments are
#' processed with [rlang::quos()] and support unquote via [`!!`] and
#' unquote-splice via [`!!!`].
#' @returns An object of class `httr2_request`.
#' @keywords internal
#' @examplesIf megamation:::has_creds()
#' megamation:::mm_req("timecard")
#' megamation:::mm_req("trade")
mm_req <- function(endpoint, ...) {
  check_string(endpoint)
  agent <- "megamation (https://github.com/asadow/megamation)"
  user <- "APIDL"
  params <- rlang::list2(...)

  if("TABLEYEAR" %in% names(params) && is.na(params$TABLEYEAR)) {
    params$TABLEYEAR <- NULL
  }

  httr2::request(mm_url()) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent(agent) |>
    httr2::req_auth_basic(user, mm_key()) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE) |>
    httr2::req_url_query(!!!params)
}

#' Append a GET request
#' @inheritParams mm_next_req
#' @param appendix `"criteria"`, `"labels"`, or `"schema"`.
#' @returns An object of class `httr2_request`.
#' @keywords internal
mm_req_append <- function(req, appendix) {
  check_string(appendix)
  httr2::req_url_path_append(req, glue::glue("@{toupper(appendix)}"))
}


