# Get the number of total pages from Megamation API response
mm_n_pages <- function(parsed) {
  sub(".*_", "", parsed$page_count) |> as.numeric()
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
#' @keywords internal
#' @examples
#' # Parses empty response body to return an empty list
#' resp <- httr2::response_json()
#' resp |> megamation:::mm_resp_parse()
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
#' `mm_parsed_extract()` extracts a data frame from the parsed
#' response body.
#'
#' @param parsed Parsed response body.
#' @param .get Whether the GET request is for the endpoint's `"data"`,
#' `"criteria"`, `"labels"`, or `"schema"`.
#' @returns A data frame containing the endpoint data.
#' @keywords internal
#' @examples
#' # Returns NULL from empty list
#' resp <- httr2::response_json()
#' resp |>
#'   megamation:::mm_resp_parse() |>
#'   megamation:::mm_parsed_extract()
mm_parsed_extract <- function(parsed, .get = "data") {
  check_string(.get)
  .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))

  switch(.get,
    data = extract_data(parsed),
    labels = ,
    criteria = extract_criteria_or_labels(parsed),
    schema = extract_schema(parsed)
  )
}

#' @rdname mm_parsed_extract
extract_data <- function(parsed) {
  parsed |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

#' @rdname mm_parsed_extract
extract_criteria_or_labels <- function(parsed) {
  description <- field <- NULL

  parsed <- parsed |> purrr::discard_at(c("Table", "Criteria", "Usage"))

  data.frame(
    field = names(parsed) |> tolower(),
    description = unlist(parsed)
  )
}

#' @rdname mm_parsed_extract
extract_schema <- function(parsed) {
  type <- description <- field <- NULL

  p <- parsed$properties
  tibble::tibble(
    field = names(p) |> tolower(),
    description = purrr::map_chr(p, "description"),
    type = purrr::map(p, "type")
  ) |>
    tidyr::unnest(type) |>
    as.data.frame()
}
