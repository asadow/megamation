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
#' @noRd
#' @examples
#' \dontrun{
#' # Real example
#' # Parses response body to return a list
#' resp <- mm_request("status") |> httr2::req_perform()
#' resp |> mm_resp_parse()
#' }
#'
#' # Fake example
#' # Parses empty response body to return an empty list
#' resp <- httr2::response_json()
#' resp |> mm_resp_parse()
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
#' @param .get Whether the GET request is for the endpoint's `"data"`,
#' `"criteria"`, `"labels"`, or `"schema"`.
#' @returns A data frame containing the endpoint data.
#' @noRd
#' @examples
#' \dontrun{
#' # Real example
#' # Returns data of interest from parsed list
#' resp <- mm_request("status") |> httr2::req_perform()
#' resp |>
#'   mm_resp_parse() |>
#'   parsed_extract()
#' }
#'
#' # Fake example
#' # Returns NULL from empty list
#' resp <- httr2::response_json()
#' resp |>
#'   mm_resp_parse() |>
#'   parsed_extract()
parsed_extract <- function(parsed, .get = "data") {
  check_string(.get)
  .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))

  switch(.get,
    data = extract_data(parsed),
    labels = ,
    criteria = extract_criteria(parsed),
    schema = extract_schema(parsed)
  )
}

#' @rdname parsed_extract
#' @noRd
extract_data <- function(parsed) {
  parsed |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1)
}

#' @rdname parsed_extract
#' @noRd
extract_criteria <- function(parsed) {
  description <- field <- NULL

  not_cols <- c("Table", "Criteria", "Usage")

  data.frame(
    field = names(parsed) |> tolower(),
    description = unlist(parsed)
  ) |>
    dplyr::filter(!field %in% !!not_cols)
}

#' @rdname parsed_extract
#' @noRd
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
