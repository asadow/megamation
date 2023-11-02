#' Format sequence of dates
#'
#' @description
#' `date_as_between_string()` formats a sequence of dates to a single string
#' that represents the sequence. The purpose is to simplify the request URL from
#' including `DATE=X&DATE=Y&DATE=Z` to `DATE = <>X,Z` where X<Y<Z.
#' @param .min A minimum Date.
#' @param .max A maximum Date.
#' @returns A single string formatted as "<>MM-DD-YYYY,MM-DD-YYYY" where the
#' "<>" modifiers means "in-between".
#' @keywords internal
#' @examples
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' date_as_between_string(date)
date_as_between_string <- function(.min, .max) {
  .min <- .min |> format("%m-%d-%Y")
  .max <- .max |> format("%m-%d-%Y")
  glue::glue("<>{.min},{.max}")
}

#' Format date values
#'
#' @description
#' `format_date()` formats given date values to be compliant with the API (
#' MM-DD-YYYY).
#' @param date A vector of type Date.
#' @returns A vector of strings.
#' @keywords internal
#' @examples
#' # Single date
#' format_date(as.Date("2023-09-20"))
#'
#' # Sequence of dates
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' format_date(date)
format_date <- function(date) {
  .min <- min(date)
  .max <- max(date)
  date_is_sequence <- identical(date, seq(.min, .max, "day")) && .min != .max

  if (!date_is_sequence) {
    format(date, "%m-%d-%Y")
  } else {
    date_as_between_string(.min, .max)
  }
}

#' Format a list of parameters
#'
#' @description
#' `format_params()` formats supplied name-value pairs toward
#' creating a valid Megamation URL.
#' @inheritParams mm_req_params
#' @returns A list of parameter name-value pairs.
#' @export
#' @examples
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' trade <- c("[]PCO", "[]DM")
#' format_params(date = date, trade = trade)
format_params <- function(...) {
  params <- rlang::list2(...)
  if (rlang::is_empty(params)) return(params)
  params <- check_params(params)

  if ("date" %in% names(params)) {
    ## Do not in-line date o.w. when check_date() errors: "params$date must be"
    date <- params$date
    check_date(date)
    params$date <- format_date(date)
  }

  names(params) <- toupper(names(params))

  max_length <- purrr::map_dbl(params, length) |> max()

  valid_list <- 1:max_length |>
    purrr::map(\(x) purrr::map(params, x)) |>
    purrr::flatten() |>
    purrr::compact() |>
    purrr::map(as.character) |>
    purrr::map(I)

  valid_list[order(names(valid_list))]
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
#' @export
#' @examples
#' # No parameters
#' mm_req("status") |> mm_req_params()
#'
#' # Multiple parameters
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' trade <- c("[]PCO", "[]DM"))
#' mm_req("status") |> mm_req_params(date = date, trade = trade)
mm_req_params <- function(req, ..., allfields = TRUE) {
  check_bool(allfields)
  params <- format_params(...)
  if (allfields) params <- c(params, "ALLFIELDS" = 1)

  req <- req |> httr2::req_url_query(!!!params)
}
