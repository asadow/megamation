#' Format a list of parameters
#'
#' @description
#' `format_params()` formats supplied name-value pairs toward
#' creating a valid and readable Megamation URL.
#' @inheritParams mm_req_params
#' @returns A list of parameter name-value pairs.
#' @keywords internal
#' @examples
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' trade <- c("[]PCO", "[]DM")
#' megamation:::format_params(date = date, trade = trade)
format_params <- function(...) {
  params <- rlang::list2(...)
  if (rlang::is_empty(params)) {
    return(params)
  }
  check_params(params)
  names(params) <- toupper(names(params))
  if ("DATE" %in% names(params)) {
    ## Do not in-line date o.w. when check_date() errors: "params$date must be"
    date <- params$DATE
    check_date(date)
    params$DATE <- format_date(date)
    tableyear <- lubridate::year(date) |> unique()
    params$TABLEYEAR <- tableyear[tableyear != lubridate::year(Sys.Date())]
  }
  return(params)
}

#' Format date values
#'
#' @description
#' `format_date()` formats given date values to be compliant with the API (
#' MM-DD-YYYY).
#' @param date A vector of type Date.
#' @returns A vector of strings.
#' @noRd
#' @examples
#' # Single date
#' megamation:::format_date(as.Date("2023-09-20"))
#'
#' # Sequence of dates
#' from <- as.Date("2023-09-20")
#' to <- as.Date("2023-10-20")
#' date <- seq(from, to, by = "day")
#' megamation:::format_date(date)
format_date <- function(date) {
  .min <- min(date)
  .max <- max(date)
  date_is_sequence <- identical(date, seq(.min, .max, "day")) && .min != .max

  if (!date_is_sequence) {
    format(date, "%m-%d-%Y")
  } else {
    date_as_between_string(.min, .max) |> I()
  }
}

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
date_as_between_string <- function(.min, .max) {
  .min <- .min |> format("%m-%d-%Y")
  .max <- .max |> format("%m-%d-%Y")
  glue::glue("<>{.min},{.max}")
}
