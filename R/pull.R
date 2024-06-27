#' Download data
#'
#' @description `mm_data()` downloads Megamation tables into R. It automatically
#'
#' 1. Creates and performs the necessary API GET request(s)
#' 2. Extracts and combines the data from the API response(s)
#' 3. Informs when no data is returned or when the API errors
#'
#' @inheritParams mm_req
#' @param allfields If `TRUE`, return all fields currently available for the
#'   endpoint.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class] containing
#'   the requested data.
#' @export
#' @examplesIf megamation:::has_creds()
#' date <- seq.Date(as.Date("2023-01-01"), as.Date("2023-01-03"), by = "day")
#' mm_data("timecard", date = date)
mm_data <- function(endpoint, ..., allfields = TRUE) {
  gets <- mm_list_get_reqs(endpoint, ..., allfields = allfields)

  cli::cli_alert_info("Requesting...\n")

  responses <- purrr::map(
    gets |>
      cli::cli_progress_along(
        format = "Fulfilling request {cli::pb_current}"),
        \(x) mm_req_perform(gets[[x]], max_reqs = Inf)
      ) |>
    purrr::list_flatten()

  errors <- responses |> httr2::resps_failures()
  errors_occured <- !rlang::is_empty(errors)

  successes <- responses |> httr2::resps_successes()
  pages <- successes |> purrr::map(mm_resp_extract)
  no_data_i <- pages |> purrr::map_lgl(is.null) |> which()

  if (errors_occured || any(no_data_i)) {
    cli::cli_alert_info(
      c("NB: {length(errors) + length(no_data_i)}/{length(responses)}",
        " request{?s} returned errors or no data.")
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

  ## Add ts timestamp column
  timestamps <- successes |>
    purrr::map(\(x) x |> purrr::pluck("headers", "Date") |> parse_header_date())
  pages <- purrr::map2(
    pages,
    timestamps,
    \(x, y) if(!is.null(x)) {
      x |> dplyr::mutate(ts = !!y)
    } else x
    )

  data <- pages |> mm_pagebind()
  data |>
    remove_api_urls() |>
    dplyr::distinct()
}
