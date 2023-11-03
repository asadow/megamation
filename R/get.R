#' Get column names and which are currently filters
#'
#' @description
#' `mm_get_names()` performs and combines two separate GET requests
#' on an endpoint: one
#' for Schema and one for Criteria. Schema returns the names and (database)
#' types of all available columns. Criteria returns the names of columns for
#' which filtering is enabled.
#'
#' @inheritParams mm_get
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested information.
#' @export
#' @examples
#' \dontrun{
#' mm_get_names("status")
#' }
mm_get_names <- function(endpoint) {
  description <- NULL
  url_ending <- c("schema", "criteria")

  req <- url_ending |>
    purrr::map(\(x) mm_request(endpoint) |> mm_req_append(x))
  resp <- purrr::map(req, httr2::req_perform)
  data <- purrr::map(resp, mm_resp_extract)
  names(data) <- url_ending

  data$schema$filter_enabled <- FALSE
  data$criteria$filter_enabled <- TRUE

  data$criteria |>
    dplyr::select(-description) |>
    dplyr::right_join(data$schema, by = "field") |>
    tibble::as_tibble()
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
#'
#' @inheritParams mm_request
#' @inheritParams mm_req_params
#' @param .paginate If `TRUE`, paginate the request.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested information.
#' @export
#' @examples
#' \dontrun{
#' # You can supply vectors to filtering variables
#' mm_get("workorder", wo_no = c("00001", "00002"))
#'
#' # You can supply API modifiers when filtering
#' mm_get("workorder", trade = "[]PCO")
#'
#' # You must supply date types to the date filter
#' jan_2023 <- seq.Date(
#'   as.Date("2023-01-01"),
#'   as.Date("2023-01-31"),
#'   by = "day"
#' )
#'
#' mm_get("employee", date = jan_2023)
#' }
mm_get <- function(endpoint, ..., .paginate = TRUE) {
  check_bool(.paginate)

  resp <- mm_request(endpoint) |>
    mm_req_params(...) |>
    mm_req_perform()

  tbl_result <- if (!.paginate) {
    resp[[1]] |>
      mm_resp_extract() |>
      tibble::as_tibble()
  } else {
    resp |>
      purrr::map(
        \(x) x |>
          mm_resp_parse() |>
          extract_data()
      ) |>
      mm_pagebind()
  }

  remove_api_urls(tbl_result)
}
