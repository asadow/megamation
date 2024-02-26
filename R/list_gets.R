#' List GET requests
#'
#' @description `mm_list_get_reqs()` returns the necessary GET request(s) for
#'   fetching the supplied endpoint and parameters. Multiple GET requests are
#'   sometimes needed for two reasons.
#'
#' One, Megamation does not support common ways of separating multiple values
#'   for a field inside an HTTP URL. These ways include:
#'
#' * using a `,`, e.g. `?x=1,2`
#' * using a `|`, e.g. `?x=1|2`
#' * turning each element into its own parameter, e.g. `?x=1&x=2`
#'
#'   For Megamation, the first two ways will result in a 404 error. The last way
#'   fetches data where x is both 1 and 2 (not 1 or 2). Hence
#'   `mm_list_get_reqs()` defines separate GET requests for each value given to
#'   a parameter.
#'
#'
#' Two, the timecard endpoint is unique in that separate GET requests must be
#'   made for separate years of data (e.g. 2022 vs 2023). This is also
#'   automatically handled by `mm_list_get_reqs()`.
#'
#' @inheritParams mm_data
#' @returns A list of GET requests of class `httr2_request`.
#' @noRd
#' @examplesIf megamation:::has_creds()
#' date <- seq.Date(as.Date("2022-01-01"), as.Date("2023-01-31"), by = "day")
#' megamation:::mm_list_get_reqs("timecard", date = date)
mm_list_get_reqs <- function(endpoint, ..., allfields = TRUE) {
  params <- format_params(endpoint, ...)
  args <- tidyr::crossing(!!!params) |>
    ## I() does not work here but we can hope
    dplyr::mutate(
      endpoint = endpoint,
      dplyr::across(dplyr::everything(), I)
    )

  if (endpoint == "timecard" && "DATE" %in% names(params)) {
    args <- add_tableyear(args)
  }

  requests <- args |>
    purrr::pmap(mm_req) |>
    purrr::list_flatten()

  if (!allfields) {
    return(requests)
  }

  requests |> purrr::map(\(req) httr2::req_url_query(req, ALLFIELDS = 1))

}

# Add the TABLEYEAR parameter which is required for past years of the "timecard"
# endpoint
add_tableyear <- function(df) {
  DATE <- NULL
  df |>
    dplyr::mutate(
      TABLEYEAR = DATE |>
        stringi::stri_extract(regex = "\\d{4}") |>
        ## Current year does not accept TABLEYEAR parameter
        dplyr::na_if(Sys.Date() |> lubridate::year() |> as.character())
    )
}
