#' Specify request options
#'
#' @param .url API base URL for request.
#' @param .key API key for request.
#' @export
#' @examples
#' req_opts(.url = "base-url")
req_opts <- function(.url = get_env_url(),
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

  structure(
    list(
      .url = .url,
      .key = .key
    ),
    class = "megamation_req_opts"
  )
}

