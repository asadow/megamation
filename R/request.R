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
#'   <[dynamic-dots]>.
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
#' * Perform the request and fetch the response using [mm_req_perform()].
#'
#' Alternatively,
#' [mm_get()] can at once define, paginate, and perform a request.
#'
#' @param endpoint The API endpoint. For example,
#' `"timecard"` for employee transactions, and
#' `"workorder"`` for work orders. All endpoints are listed at
#' "https://apidocs.megamation.com/".
#' @param ... <[dynamic-dots]> Name-value pairs to filter the request.
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
#' @param .get A length-1 character vector representing whether the request is
#' for the endpoint's `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .url The API URL.
#' @param .key The API key.
#' @returns An object of class `httr2_request`.
#' @export
mm_request <- function(endpoint,
                       ...,
                       .get = "data",
                       .url = get_env_url(),
                       .key = get_env_key()) {
  .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))

  CSoL <- switch(.get,
                 criteria = "CRITERIA",
                 schema = "SCHEMA",
                 labels = "LABELS"
  )

  params <- list(...)
  names(params) <- toupper(names(params))

  request <- httr2::request(.url) |>
    httr2::req_url_path_append(endpoint)

  if(!is.null(CSoL)) {
    request <- request |>
      httr2::req_url_path_append(glue::glue("@{CSoL}"))
    }

  agent <- "megamation (https://github.com/asadow/megamation)"

  request |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(agent) |>
    httr2::req_auth_basic("APIDL", .key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}

#' Perform a GET request to Megamation's API
#'
#' @description
#' `mm_get()` accomplishes the full process of a GET request:
#'
#' * Creates an API request and defines its behaviour.
#' * Performs the request and fetches the response.
#' * Converts the body of the response to a data frame.
#'
#' Where applicable, pagination is automatically applied to the request
#' by [mm_req_paginate()] and returned pages are automatically combined.
#'
#' @inheritParams mm_request
#' @returns A data frame of class [tbl] containing the requested information.
#' @export
mm_get <- function(endpoint,
                   ...,
                   .get = "data",
                   .url = get_env_url(),
                   .key = get_env_key(),
                   .full_resp = C(FALSE, TRUE)) {
  .get <- rlang::arg_match(.get, c("data", "criteria", "labels", "schema"))

  req <- mm_request(endpoint, ..., .get = .get, .url = .url, .key = .key)

  paginatable <- .get == "data"

  result <- if(!paginatable) {
    resp <- req |>
      httr2::req_perform()
    resp |>
      resp_body_parse() |>
      parsed_to_tbl(.get)
  } else{
    list_of_resp <- req |>
      mm_req_paginate() |>
      httr2::paginate_req_perform()
    list_of_resp |>
      map(
        \(x) x |>
          resp_body_parse() |>
          mm_keep_df(x)
      ) |>
      mm_bind_then_tbl()
  }

  return(result)

}

