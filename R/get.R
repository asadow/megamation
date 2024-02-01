#' Get data
#'
#' @description
#' Download Megamation data you own via the API and import the data into R.
#'
#' `mm_get()` accomplishes a variety of conveniences when fetching data from
#' Megamation. It creates the necessary API GET requests, performs them,
#' converts response bodies to data frames, and finally binds them
#'
#' @inheritParams mm_req
#' @param allfields If `TRUE`, return all fields currently available for
#' the endpoint.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested data.
#' @export
#' @examplesIf megamation:::has_creds()
#' # Get all timecard entries from Jan. 2023:
#'
#' # Create a Date-type vector
#' jan_2023 <- seq.Date(
#'   as.Date("2023-01-01"),
#'   as.Date("2023-01-31"),
#'   by = "day"
#' )
#'
#' mm_get("timecard", date = jan_2023)
mm_get <- function(endpoint, ..., allfields = TRUE) {
  gets <- mm_define_gets(endpoint, ..., allfields = allfields)
  pages <- gets |>
    purrr::map(mm_req_perform) |>
    ## We get a list of pages per request
    purrr::list_flatten() |>
    purrr::map(mm_resp_extract)

  data <- pages |> mm_pagebind()

  if (nrow(data) == 0) {
    cli::cli_alert_info("No data returned.")
    return()
  }
  remove_api_urls(data)
}

#' Define GET requests
#'
#' @description
#' `mm_define_gets()` will
#' return the necessary GET requests for fetching the supplied endpoint and
#' parameters.
#' Multiple GET requests are sometimes necessary for two reasons.
#'
#' One, Megamation does not support common ways of separating multiple
#' values for a field inside an HTTP URL. These ways include:
#'
#' * using a `,`, e.g. `?x=1,2`
#' * using a `|`, e.g. `?x=1|2`
#' * turning each element into its own parameter, e.g. `?x=1&x=2`
#'
#' For Megamation, the first two ways will result in a 404 error.
#' The last way fetches data where x is both 1 and 2 (not 1 or 2).
#' Hence this function creates separate GET requests for each separate value
#' supplied to a parameter.
#'
#' The second reason for multiple GET requests regards the timecard endpoint,
#' which is unique in that separate GET requests must be made for separate years
#' of data (e.g. 2022 vs 2023). This is also automatically handled by
#' `mm_define_gets()`.
#'
#' @inheritParams mm_get
#' @returns A list of GET requests of class `httr2_request`.
#' @export
#' @examplesIf megamation:::has_creds()
#' # Get timecard entries from Jan. 2022 to 2023:
#'
#' # Create a Date-type vector
#' date <- seq.Date(
#'   as.Date("2022-01-01"),
#'   as.Date("2023-01-31"),
#'   by = "day"
#' )
#'
#' mm_define_gets("timecard", date = date)
mm_define_gets <- function(endpoint, ..., allfields = TRUE) {
  params <- rlang::list2(...)
  list_args <- if (rlang::is_empty(params)) {
    tibble::tibble(endpoint = endpoint) |> list()
  } else
    mm_args_for_gets(endpoint, !!!params)

  requests <- list_args |>
    purrr::map(\(x) purrr::pmap(x, mm_req)) |>
    purrr::list_flatten()

  if (!allfields) {
    return(requests)
  } else {
    requests |> purrr::map(\(req) httr2::req_url_query(req, ALLFIELDS = 1))
  }
}

#' Get arguments for GET request(s)
#'
#' @description
#' For fetching a specified endpoint with parameters, `mm_args_for_gets()` will
#' return the argument dataframes needed for any GET requests.
#'
#' @return A list of dataframes where each column is an argument
#' to [mm_req()].
#' @inheritParams mm_get
#' @examples
#' # Return arguments needed for timecard GET for Jan. 2022 to 2023:
#'
#' # Create a Date-type vector
#' date <- seq.Date(
#'   as.Date("2022-01-01"),
#'   as.Date("2023-01-31"),
#'   by = "day"
#' )
#'
#' mm_args_for_gets("timecard", date = date)
mm_args_for_gets <- function(endpoint, ...) {
  params <- rlang::list2(...)

  requesting_date <- !is.null(params$date)
  if (requesting_date) {
    years <- lubridate::year(params$date) |> unique()
  }

  params <- format_params(!!!params)
  args <- purrr::map(params, \(x) tidyr::crossing(!!!x)) |>
    purrr::list_rbind() |>
    ## Why doesn't I() work?
    dplyr::mutate(
      dplyr::across(dplyr::everything(), I),
      endpoint = endpoint
    )

  if (!requesting_date) {
    return(list(args))
  }

  ## TABLEYEAR is needed for past timecard years
  current_year <- lubridate::year(Sys.Date())
  this_years_timecard <- endpoint == "timecard" &&
    identical(current_year, years)

  list_args <- if (endpoint != "timecard" || this_years_timecard) {
    return(list(args))
  } else {
    ## Request includes timecard past years, so we add TABLEYEAR
    args <- args |>
      dplyr::mutate(TABLEYEAR = DATE |> stringr::str_extract("\\d{4}"))

    ## Request may also include this year's timecard, so we separate args
    args_this_year <- args |>
      dplyr::filter(TABLEYEAR == !!current_year) |>
      dplyr::select(- TABLEYEAR)

    args_past_years <- args |>
      dplyr::filter(TABLEYEAR != !!current_year)

    list(args_this_year, args_past_years)
  }
  list_args
}
