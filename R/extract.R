#' Extract data from Megamation API response
#'
#' `mm_resp_extract()` parses the raw bytes from an API response,
#' and extracts data from the parsed object.
#'
#' @param resp An API response.
#' @description The body of the response contains raw bytes.
#' After converting these bytes to a string, encoding is done to resolve
#' a UTF-8 issue from Megamation's side.
#' @returns A data frame containing the endpoint data.
#' @keywords internal
#' @examples
#' # Returns NULL from an empty response body
#' resp <- httr2::response_json()
#' resp |> megamation:::mm_resp_extract()
mm_resp_extract <- function(resp) {
  .from <- sub(".*/@", "", resp$url) |> tolower()
  .from <- switch(.from,
    "data",
    labels = "labels",
    criteria = "criteria",
    schema = "schema"
  )
  resp |>
    mm_resp_parse() |>
    mm_parsed_extract(.from)
}
