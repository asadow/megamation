#' Append a GET request
#' @inheritParams mm_next_req
#' @param appendix `"criteria"`, `"labels"`, or `"schema"`.
#' @returns An object of class `httr2_request`.
#' @keywords internal
mm_req_append <- function(req, appendix) {
  check_string(appendix)
  httr2::req_url_path_append(req, glue::glue("@{toupper(appendix)}"))
}

#' Modify request URL with filtering components
#'
#' `mm_req_params()` adds filters to the request. By default, it adds the query
#' for all (currently available) fields.
#'
#' @inheritParams mm_next_req
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name-value pairs to filter the
#' request. The name should be the lower-case name of a field that is
#' filter-enabled (in Megamation's words, a criteria). These arguments are
#' processed with [rlang::quos()] and support unquote via [`!!`] and
#' unquote-splice via [`!!!`].
#' @param allfields If `TRUE`, return all fields currently available for
#' the endpoint.
#' @returns An object of class `httr2_request` with a pagination policy.
#' @keywords internal
mm_req_params <- function(req, ..., allfields = TRUE) {
  check_bool(allfields)
  params <- format_params(...)
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

# Waiting for httr2 bug to be fixed ---------------------------------------
  ## https://github.com/r-lib/httr2/issues/404
  ## Can then remove I() from utils-params.R format_date()
  ## Can also remove line below and place in format_params()
  # params <- params |> purrr::map(I)
  httr2::req_url_query(req, !!!params, .multi = "comma")
}
