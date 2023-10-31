#' Specify request options
#'
#' for the endpoint's `"data"`, `"criteria"`, `"labels"`, or `"schema"`.
#' @param .url API base URL for request.
#' @param .key API key for request.
#' @param .paginate If `TRUE`, paginate the request.
#' @export
req_opts <- function(.get = "data",
                     .url = get_env_url(),
                     .key = get_env_key(),
                     .paginate = TRUE) {
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
  check_string(.get)
  .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))

  structure(
    list(
      .get = .get,
      .url = .url,
      .key = .key,
      .paginate = .paginate
    ),
    class = "megamation_req_opts"
  )
}

