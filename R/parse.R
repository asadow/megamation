#' Extract data from Megamation API response
#'
#' `mm_resp_extract()` parses the raw bytes from an API response,
#' and extracts data from the parsed object.
#'
#' @param resp An API response.
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#' @returns A list.
#' @export
mm_resp_extract <- function(resp) {
  .from <- sub(".*/@", "", resp$url) |> tolower()

  resp |>
    mm_resp_parse() |>
    parsed_extract(.from)
}

#' Parse body from Megamation API response
#'
#' `mm_resp_parse()` parses the raw bytes from an API response,
#' returning an R list object. After converting bytes to characters,
#' encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#'
#' @param resp An API response.
#' @returns A list.
#' @export
mm_resp_parse <- function(resp) {
  resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    stringi::stri_encode(from = "UTF-8", to = "UTF-8") |>
    jsonlite::fromJSON()
}

#' Create tibble from parsed response
#'
#' @description
#'
#' `parsed_extract()` extracts a data frame from the parsed
#' response body.
#'
#' @param parsed Parsed response body.
#' @inheritParams mm_request
#' @returns A data frame containing the endpoint data.
#'
#' @export
parsed_extract <- function(parsed,
                          .get = c("data", "criteria", "labels", "schema")) {
  .get <- rlang::arg_match(.get)

  switch(
    .get,
    data = extract_data(parsed),
    labels = ,
    criteria = extract_criteria(parsed),
    schema = extract_schema(parsed)
  )
}

#' @rdname parsed_extract
#' @export
extract_data <- function(parsed) {
  parsed |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

#' @rdname parsed_extract
#' @export
extract_criteria <- function(parsed) {
  not_cols <- c("Table", "Criteria", "Usage")

  data.frame(
    field = names(parsed) |> tolower(),
    description = unlist(parsed)
  ) |>
    dplyr::filter(!field %in% !!not_cols)
}

#' @rdname parsed_extract
#' @export
extract_schema <- function(parsed) {
  p <- parsed$properties

  tibble::tibble(
    field = names(p) |> tolower(),
    description = purrr::map_chr(p, "description"),
    type = purrr::map(p, "type")
  ) |>
    tidyr::unnest(type) |>
    as.data.frame()
}

