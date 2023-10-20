#' Parse body from Megamation API response
#'
#' `body_parse()` converts the raw bytes of an API response to an R list object.
#' After converting bytes to characters, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#'
#' @param resp_body An API response body.
#' @returns A list.
#' @export
body_parse <- function(resp_body) {
  resp_body |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON()
}

#' Extract and parse body from Megamation API response
#'
#' `resp_body_parse()` extracts the raw bytes from an API response and parses
#' it, returning an R list object.
#'
#' @param resp An API response.
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#' @returns A list.
#' @export
resp_body_parse <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    body_parse()
}

#' Create tibble from parsed response
#'
#' `parsed_to_tbl()` constructs a data frame of class [tbl_df].
#' The input should be a list returned from parsing
#' the API's response body.
#'
#' @param parsed Parsed response body.
#' @inheritParams mm_request
#' @returns A data frame of class [tbl_df] containing either
#' * Columns of the endpoint's data.
#' * Columns representing the list given by the endpoint's criteria, labels,
#' or schema: `list_name` and `value`.
#'
#' @export
parsed_to_tbl <- function(parsed,
                          .get = c("data", "criteria", "labels", "schema")) {
  .get <- rlang::arg_match(.get)

  switch(
    .get,
    data = to_tbl_data(parsed),
    labels = ,
    criteria = to_tbl_criteria(parsed),
    schema = to_tbl_schema(parsed)
  )
}

#' Create tibble from parsed response to Get Schema
#' @inheritParams parsed_to_tbl
#' @returns A [tbl_df] with columns `list_name` and `value`
#' representing the Schema response list. `value` is
#' `value` is of type `list` because of the lists contained in the values
#' indexed by the `required` and `properties` list names.
#' @export
to_tbl_schema <- function(parsed) {

  properties <- parsed$properties

  tbl <- tibble::tibble(
    list_name = names(parsed),
    value = unname(parsed)
  ) |>
    dplyr::filter(list_name != "properties")

  properties <- tibble::tibble(
    column = names(properties),
    description = purrr::map_chr(properties, "description"),
    type = purrr::map(properties, "type")
  ) |>
    tidyr::unnest(type)

  tbl |>
    dplyr::add_row(
      list_name = "col_properties",
      value = list(properties)
    )
}

#' Create tibble from parsed response

#' @inheritParams parsed_to_tbl
#' @returns A [tbl_df] of the [data.frame] object contained in the
#' parsed response. Columns are unaltered from the endpoint's data.
#' @export
to_tbl_data <- function(parsed) {
  parsed |>
    mm_keep_df() |>
    tibble::as_tibble()
}

#' Create tibble from parsed response to Get Criteria or Get Labels
#' @inheritParams parsed_to_tbl
#' @returns A [tbl_df] with columns `list_name` and `value`
#' representing the Criteria or Labels response list.
#' @export
to_tbl_criteria <- function(parsed) {
  tibble::tibble(
    list_name = names(parsed),
    value = unlist(parsed)
  )
}

#' Keep only data from parsed response
#'
#' `mm_keep_df()` extracts a data frame from a multi-layer list
#' (parsed body of an API response).
#'
#' @inheritParams parsed_to_tbl
#' @export
#' @returns The [data.frame] object contained in the
#' parsed response.
#' @keywords internal
mm_keep_df <- function(parsed) {
  parsed |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

