#' Perform a Megamation API request
#' @param url A URL starting with "https://api.megamation.com/".
#' @param resource The resource endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param username The API key username.
#' @param api_key The API key.
#' @export

mm_req <- function(url, resource, ..., username = "APIDL", api_key = get_api_key()) {
  params <- list(...)
  names(params) <- toupper(names(params))

  httr2::request(url) |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("megamation (https://github.com/asadow/megamation)") |>
    httr2::req_auth_basic(username, api_key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE) |>
    httr2::req_perform()
}
