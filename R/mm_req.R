#' Perform a Megamation API request
#' @param resource The resource endpoint. For example,
#' "timecard" for employee transactions, and
#' "workorder" for work orders. All endpoints are listed here:
#' "https://apidocs.megamation.com/".
#' @param ... API parameters to filter the request.
#' @param user The API key user.
#' @param api_key The API key.
#' @export

mm_req <- function(resource,
                   ...,
                   url = get_url(),
                   user = get_user(),
                   key = get_key()) {
  params <- list(...)
  names(params) <- toupper(names(params))

  httr2::request(url) |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("megamation (https://github.com/asadow/megamation)") |>
    httr2::req_auth_basic("APIDL", key) |>
    httr2::req_error(body = mm_error_body) |>
    httr2::req_cache(tempdir(), debug = TRUE)
}
