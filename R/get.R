#' Get column names, filter statuses, descriptions, and types
#'
#' @description
#' `mm_get_col_info()` returns column names, filter status, description,
#' and types. It does so by combining the results of the criteria and schema
#' appendices.
#'
#' `mm_get_criteria()` returns column names and filter status.
#'
#' `mm_get_schema()` returns column names, descriptions, and types.
#'
#' `mm_get_labels()` returns column names and descriptions.
#'
#' @inheritParams mm_get
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the endpoint's appendix.
#' @export
#' @examplesIf megamation:::has_creds()
#' mm_get_col_info("status")
mm_get_col_info <- function(endpoint) {
  description <- filter_enabled <- NULL
  data_criteria <- mm_get_criteria(endpoint)
  data_schema <- mm_get_schema(endpoint)
  data_criteria$filter_enabled <- TRUE

  data_criteria |>
    dplyr::select(- description) |>
    dplyr::right_join(data_schema, by = "field") |>
    tibble::as_tibble() |>
    dplyr::mutate(filter_enabled = ifelse(is.na(filter_enabled), FALSE, TRUE))
}

#' Get endpoint appendix (criteria, schema, labels)
#'
#' `mm_get_appendix()` returns the user-supplied appendix.
#'
#' @inheritParams mm_get
#' @inheritParams mm_req_append
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the endpoint's appendix.
#' @keywords internal
mm_get_appendix <- function(endpoint, appendix) {
  mm_request(endpoint) |>
    mm_req_append(appendix) |>
    httr2::req_perform() |>
    mm_resp_extract() |>
    tibble::as_tibble()
}

#' @rdname mm_get_col_info
#' @export
mm_get_criteria <- function(endpoint) {
  mm_get_appendix(endpoint, "criteria")
}

#' @rdname mm_get_col_info
#' @export
mm_get_schema <- function(endpoint) {
  mm_get_appendix(endpoint, "schema")
}

#' @rdname mm_get_col_info
#' @export
mm_get_labels <- function(endpoint) {
  mm_get_appendix(endpoint, "labels")
}

#' Get data
#'
#' @description
#' `mm_get()` accomplishes the full process of a GET request:
#'
#' * Creates an API request and defines its behaviour.
#' * Performs the request and fetches the response.
#' * Converts the body of the response to a data frame.
#'
#' Where applicable, pagination is automatically applied to the request
#' and returned pages are automatically combined.
#' @inheritParams mm_req_params
#' @inheritParams mm_request
#' @param .paginate If `TRUE`, paginate the request.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested data.
#' @export
#' @examplesIf megamation:::has_creds()
#' # Get all work orders from Jan. 2023 containing trades ADMIN and IT:
#'
#' # First create a Date-type vector
#' jan_2023 <- seq.Date(
#'   as.Date("2023-01-01"),
#'   as.Date("2023-01-31"),
#'   by = "day"
#' )
#'
#' # Then prefix the trade values with the "containing" modifier "[]"
#' # since trade is a list column in the work order table
#' admin_and_it <- c("[]ADMIN", "[]IT")
#'
#' mm_get("workorder", date = jan_2023, trade = admin_and_it)
mm_get <- function(endpoint, ..., .paginate = TRUE) {
  check_bool(.paginate)
  params <- rlang::list2(...)

  resp <- mm_request(endpoint) |>
    mm_req_params(!!!params) |>
    mm_req_perform()

  page_1 <- resp[[1]] |> mm_resp_extract()

  if (is.null(page_1)) {
    cli::cli_alert_info("No data returned.")
    return(tibble::tibble())
  }

  tbl_result <- if (!.paginate) {
    page_1 |> tibble::as_tibble()
  } else {
    pages <- resp |> purrr::map(\(x) mm_resp_extract(x))
    pages |> mm_pagebind()
  }

  remove_api_urls(tbl_result)
}
