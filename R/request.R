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
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#' mm_req("timecard")
#' mm_req("trade")
mm_req <- function(endpoint, .url = get_env_url(), .key = get_env_key()) {
  check_string(endpoint)
  check_key(.key)

  httr2::request(.url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent(
      "megamation (https://github.com/asadow/megamation)"
    ) |>
    httr2::req_auth_basic("APIDL", .key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}

#' Append a GET request
#' @inheritParams mm_req_paginate
#' @param x `"criteria"`, `"labels"`, or `"schema"`.
#' @returns An object of class `httr2_request`.
#' @export
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#' mm_req("workorder") |> mm_req_append("criteria")
#'
mm_req_append <- function(req, x) {
  check_string(x)
  .get <- rlang::arg_match(x, c("criteria", "labels", "schema"))

  req |>
    httr2::req_url_path_append(glue::glue("@{toupper(x)}"))
}

#' Modify request URL with filtering components
#'
#' `mm_req_params()` adds filters to the request. By default, it adds the query
#' for all (currently available) fields.
#'
#'
#' @inheritParams mm_req_paginate
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs to filter the request.
#' The name should be the lower-case name of a
#' field that is filter-enabled
#' (in Megamation's words, a criteria).
#' @param allfields If `TRUE`, return all fields currently available for
#' the endpoint.
#' @returns An object of class `httr2_request` with a pagination policy.
#' @export
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#' # No parameters
#' mm_req("status") |> mm_req_params()
#'
#' # Multiple parameters
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' trade <- c("[]PCO", "[]DM")
#' mm_req("status") |> mm_req_params(date = date, trade = trade)
mm_req_params <- function(req, ..., allfields = TRUE) {
  check_bool(allfields)
  params <- format_params(...)
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

  req <- req |> httr2::req_url_query(!!!params)
}
