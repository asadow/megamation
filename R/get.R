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

  cli::cli_alert_info("Requesting...")

  responses <- purrr::map(
    gets |>
      cli::cli_progress_along(format = "Fulfilling request {cli::pb_current}"),
    \(x) mm_req_perform(gets[[x]])
      )  |>
    purrr::list_flatten()

  errors <- responses |> httr2::resps_failures()
  errors_occured <- !rlang::is_empty(errors)

  successes <- responses |> httr2::resps_successes()
  pages <- successes |> purrr::map(mm_resp_extract)

  no_data_i <- pages |> purrr::map_lgl(is.null) |> which()

  if (errors_occured || any(no_data_i)) {
    cli::cli_alert_info(
      c("NB: {length(errors) + length(no_data_i)}/{length(responses)}",
        " requests returned errors or no data.")
      )
    for (error in errors) {
      url <- error$request$url
      msg <- error$message
      cli::cli_h1("Errored")
      glue::glue("{msg} \n {cli::style_bold('GET')} {url}") |>
        cli::cli_alert_danger()
    }

    for (i in no_data_i) {
      url <- successes[[i]]$request$url
      cli::cli_h1("No Data Returned")
      glue::glue("\n {cli::style_bold('GET')} {url}") |>
        cli::cli_alert_danger()
    }
  }

  data <- pages |> mm_pagebind()
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

# Add the TABLEYEAR parameter which is required for past years of the
# "timecard" endpoint
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
