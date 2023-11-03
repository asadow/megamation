#' Append a GET request
#' @inheritParams mm_req_paginate
#' @param x `"criteria"`, `"labels"`, or `"schema"`.
#' @returns An object of class `httr2_request`.
#' @export
#' @examplesIf httr2::secret_has_key("HTTR2_KEY_MEGAMATION")
#' mm_request("workorder") |> mm_req_append("criteria")
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
#' @examplesIf httr2::secret_has_key("HTTR2_KEY_MEGAMATION")
#' # No parameters
#' mm_request("status") |> mm_req_params()
#'
#' # Multiple parameters
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' trade <- c("[]PCO", "[]DM")
#' mm_request("status") |> mm_req_params(date = date, trade = trade)
mm_req_params <- function(req, ..., allfields = TRUE) {
  check_bool(allfields)
  params <- format_params(...)
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

  req <- req |> httr2::req_url_query(!!!params)
}
