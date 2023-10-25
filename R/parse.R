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
#' @description
#'
#' `parsed_to_tbl()` constructs a data frame of class [tbl_df].
#' The input should be the parsed API response body list.
#'
#'  `parsed_to_tbl()` uses the following methods:
#'
#' * `to_tbl_data()` converts the Get Data parsed body, but only keeps the
#' embedded data frame.
#' * `to_tbl_criteria()` converts the Get Criteria or Labels parsed body.
#' * `to_tbl_schema()` converts the Get Schema parsed body.
#'
#'
#' @param parsed Parsed response body.
#' @inheritParams mm_request
#' @returns A data frame of class [tbl_df] containing either
#' * Columns of the endpoint data.
#' * Columns `list_name` and `value` representing the list given by the
#' endpoint's Criteria, Labels, or Schema. If Schema,
#' `value` is of type `list` because of the lists under names `required` and
#' `properties`.
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

#' @rdname parsed_to_tbl
#' @export
to_tbl_data <- function(parsed) {
  parsed |>
    parsed_keep_df() |>
    tibble::as_tibble()
}

#' @rdname parsed_to_tbl
#' @export
to_tbl_criteria <- function(parsed) {
  tibble::tibble(
    list_name = names(parsed),
    value = unlist(parsed)
  )
}

#' @rdname parsed_to_tbl
#' @export
to_tbl_schema <- function(parsed) {

  list_name <- type <- NULL

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

#' @rdname parsed_to_tbl
#' @export
parsed_keep_df <- function(parsed) {
  parsed |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

