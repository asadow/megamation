#' Define a Megamation API request
#' @param endpoint The endpoint endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... Name-value pairs to filter the request.
#' The name should be the lower-case name of a
#' field that is filter-enabled
#' (that is, a criteria in Megamation's words).
#'
#' The value should can be:
#'
#' * A character value of said variable.
#' * An as-is, modifier-value(s) combination,
#'   e.g. `I("<>2020-01-01,2020-12-31")`.
#'
#' To filter a single variable by multiple values, repeat name-value pairs for
#' the same name but different values. e.g. `trade = "DM", trade = "PCO"`.
#' @param .give What the response should give:
#' `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .url The API URL.
#' @param .key The API key.
#' @export

mm_req <- function(endpoint,
                   ...,
                   .give = "data",
                   .url = get_url(),
                   .key = get_key()) {

  .give <- rlang::arg_match(.give, c("criteria", "labels", "schema", "data"))

  CSoL <- switch(.give,
                 criteria = "CRITERIA",
                 schema = "SCHEMA",
                 labels = "LABELS"
  )

  params <- list(...)
  names(params) <- toupper(names(params))

  req <- httr2::request(.url) |>
    httr2::req_url_path_append(endpoint)

  if(!is.null(CSoL)) {
    req <- req |>
      httr2::req_url_path_append(glue::glue("@{CSoL}"))
    }

  agent <- "megamation (https://github.com/asadow/megamation)"

  req |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(agent) |>
    httr2::req_auth_basic("APIDL", .key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}
