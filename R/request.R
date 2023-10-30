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
#' @returns An object of class `httr2_request`.
#' @export
mm_req <- function(endpoint, opts = req_opts()) {
  if (!inherits(opts, "megamation_req_opts")) {
    cli::cli_abort("{.arg opts} must be created by {.fun req_opts}.")
  }

  req <- req |>
    httr2::req_user_agent(
      "megamation (https://github.com/asadow/megamation)"
    ) |>
    httr2::req_auth_basic("APIDL", opts$.key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}

#' Create a Megamation API request
#'
#' @description
#' `mm_request()` creates a request using [httr2::request()] and
#' does the following:
#'
#' * Inserts the base URL using the environment variable `MEGAMATION_URL` from
#'  your `.Renviron`. Your key and base URL can be set using [mm_set_creds()].
#' * Appends the URL with the endpoint defined by parameter `endpoint`.
#' * Appends the URL with the filtering defined by parameter `...`
#'   <[`dynamic-dots`][rlang::dyn-dots]>.
#' * Sets the user-agent as the GitHub `megamation` package.
#' * Authenticates the request with HTTP basic authentication using
#'   environment variables `MEGAMATION_KEY` and `MEGAMATION_URL`
#'   from your `.Renviron`.
#' * Handles HTTP errors so useful information from the response is extracted
#'   (e.g. "No response body").
#' * Adds caching of responses if available. See [httr2::req_cache()].
#'
#' If you create a GET request with `mm_request()`, you can then
#'
#' * Paginate the request using [mm_req_paginate()].
#' * Perform the request and fetch the response using [httr2::req_perform()] or
#' [httr2::req_perform_iteratively()] if the request is paginated.
#'
#' Alternatively,
#' [mm_get()] can at once define, paginate, and perform a GET request.
#'
#' @param endpoint The API endpoint. For example,
#' `"timecard"` for employee transactions, and `"workorder"`
#' for work orders. All endpoints are listed at
#' https://apidocs.megamation.com/.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs to filter the request.
#' The name should be the lower-case name of a
#' field that is filter-enabled
#' (in Megamation's words, a criteria).
#' @param allfields If `TRUE`, return all fields set to be available for
#' the endpoint.
#' @returns An object of class `httr2_request`.
#' @export
mm_request <- function(endpoint, ..., allfields = TRUE, opts = req_opts()) {
  check_bool(allfields)
  if (!inherits(opts, "megamation_req_opts")) {
    cli::cli_abort("{.arg opts} must be created by {.fun req_opts}.")
  }

  params <- format_params(...)
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

  CSoL <- switch(
    opts$.get,
    criteria = "CRITERIA",
    schema = "SCHEMA",
    labels = "LABELS"
    )

  req <- httr2::request(opts$.url) |>
    httr2::req_url_path_append(endpoint)

  if(!is.null(CSoL)) {
    req <- req |>
      httr2::req_url_path_append(glue::glue("@{CSoL}"))
    }

  req <- req |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(
      "megamation (https://github.com/asadow/megamation)"
    ) |>
    httr2::req_auth_basic("APIDL", opts$.key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)

}


