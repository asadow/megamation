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
#' * Automatic caching of responses. See [httr2::req_cache()].
#'
#' After creating a request with `mm_request()`, you can
#'
#' * Paginate the request using [mm_req_paginate()].
#' * Perform the request and fetch the response using [httr2::req_perform()] or
#' [httr2::req_perform_iteratively()] if the request is paginated.
#'
#' Alternatively,
#' [mm_get()] can at once define, paginate, and perform a request.
#'
#' @param endpoint The API endpoint. For example,
#' `"timecard"` for employee transactions, and `"workorder"`
#' for work orders. All endpoints are listed at
#' https://apidocs.megamation.com/.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs to filter the request.
#' The name should be the lower-case name of a
#' field that is filter-enabled
#' (in Megamation's words, a criteria).
#'
#' The value should be:
#'
#' * A length-1 atomic vector representing the value of said variable.
#' * An as-is, modifier-value(s) combination,
#'   e.g. `I("<>2020-01-01,2020-12-31")`. [I()] is used to prevent automatic
#'   escaping.
#'
#' To filter a single variable by multiple values, repeat name-value pairs with
#' the same name but different values. e.g. `trade = "DM", trade = "PCO"`.
#' @param allfields If `TRUE`, return all fields set to be available for
#' the endpoint.
#' @param .get A length-1 character vector representing whether the request is
#' for the endpoint's `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .paginate If `TRUE`, paginate the request.
#' @param .url The API URL.
#' @param .key The API key.
#' @returns An object of class `httr2_request`.
#' @export
mm_request <- function(endpoint,
                       ...,
                       allfields = TRUE,
                       .get = "data",
                       .paginate = TRUE,
                       .url = get_env_url(),
                       .key = get_env_key()) {
  if(.key != get_env_key()) {
    cli::cli_warn(c(
      "The {.arg .key} you provided is not your
      MEGAMATION_KEY environment variable.",
      "i" = "It is highly recommended that you run {.fun mm_set_creds},
      and {.emph do not} supply {.arg .key}.",
      "i" = 'A typo like `kee = <your-secret>`
      will end up in the request URL as a filter.'
    ))
  }
  check_bool(allfields)
  check_string(.get)
  .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))

  params <- format_params(...)
  allfields <- .paginate <- .get == "data"
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

  CSoL <- switch(
    .get,
    criteria = "CRITERIA",
    schema = "SCHEMA",
    labels = "LABELS"
    )

  req <- httr2::request(.url) |>
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
    httr2::req_auth_basic("APIDL", .key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)

  if(!.paginate) {
    return(req)
  }

  mm_req_paginate(req)
}


