#' Retrieve column names and details
#'
#' @description `mm_names()` returns column names, filter status, description,
#'   and types through GET requests of the criteria and schema appendices.
#'
#' @inheritParams mm_data
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class] containing
#'   the endpoint's appendix.
#' @export
#' @examplesIf megamation:::has_creds()
#' mm_names("status")
mm_names <- function(endpoint) {
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
#' @inheritParams mm_data
#' @inheritParams mm_req_append
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class] containing
#'   the endpoint's appendix.
#' @keywords internal
mm_get_appendix <- function(endpoint, appendix) {
  mm_req(endpoint) |>
    mm_req_append(appendix) |>
    httr2::req_perform() |>
    mm_resp_extract() |>
    tibble::as_tibble()
}

# `Column names and filter status
mm_get_criteria <- function(endpoint) {
  mm_get_appendix(endpoint, "criteria")
}

# Column names, descriptions, and types
mm_get_schema <- function(endpoint) {
  mm_get_appendix(endpoint, "schema")
}

# Column names and descriptions
mm_get_labels <- function(endpoint) {
  mm_get_appendix(endpoint, "labels")
}
