#' Append a GET request
#' @inheritParams mm_req_paginate
#' @param x `"criteria"`, `"labels"`, or `"schema"`.
#' @export
mm_req_append <- function(req, x) {
  check_string(x)
  .get <- rlang::arg_match(x, c("criteria", "labels", "schema"))

  req |>
    httr2::req_url_path_append(glue::glue("@{toupper(x)}"))
}

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
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#' mm_get_names("status")
#' @export
mm_get_names <- function(endpoint) {
  url_ending <- c("schema", "criteria")

  req <- url_ending |>
    purrr::map(\(x) mm_req(endpoint) |> mm_req_append(x))
  resp <- purrr::map(req, httr2::req_perform)
  data <- purrr::map(resp, mm_resp_extract)
  names(data) <- url_ending

  data$schema$filter_enabled <- FALSE
  data$criteria$filter_enabled <- TRUE

  data$criteria |>
    dplyr::select(- description) |>
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
#' by [mm_req_paginate()] and returned pages are automatically combined.
#'
#' @inheritParams mm_req
#' @param .paginate If `TRUE`, paginate the request.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class]
#' containing the requested information.
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#'
#' # For status endpoint
#'
#' mm_get("status")
#'
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
#'   )
#'
#' mm_get("employee", date = jan_2023)
#' }
#'
#'
#' @export
mm_get <- function(endpoint, ..., .paginate = TRUE) {
  check_bool(.paginate)

  req <- mm_req(endpoint) |> mm_req_params(...)

  req <- if (!.paginate) {
    req
  } else mm_req_paginate(req)

  resp <- mm_req_perform(req)

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
      mm_bind_then_tbl()
  }

  remove_api_urls(tbl_result)
}

#' Perform a Megamation API request
#'
#' @description
#'
#' After creating a request with [mm_req()],
#' call `mm_req_perform()` to perform it and fetch
#' the results back to R.
#'
#' Where the request is paginated, pagination is automatically performed
#' by [httr2::req_perform_iteratively()].
#'
#' @inheritParams mm_req_paginate
#' @returns A list of HTTP responses. Each response is
#' an S3 list with class `httr2_response`. (For more on this class,
#' see [httr2::response].) If the request was paginated, these
#' responses correspond to pages.
#' @examplesIf httr2::secret_has_key("HTTR2_KEY")
#' @export
mm_req_perform <- function(req) {
  check_request(req)

  if (!is_paginated(req)) {
    req |>
      httr2::req_perform() |>
      list()
  } else {
    req |>
      httr2::req_perform_iteratively()
  }
}