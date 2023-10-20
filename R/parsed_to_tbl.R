#' Create tibble(s) from parsed Megamation API response.
#' @param parsed Parsed Megamation API response body.
#' @export
parsed_to_tbl <- function(parsed,
                          .for = c("data", "criteria", "labels", "schema")) {
  .for <- rlang::arg_match(.for)

  switch(
    .for,
    data = to_tbl_data(parsed),
    labels = ,
    criteria = to_tbl_criteria(parsed),
    schema = to_tbl_schema(parsed)
  )
}

to_tbl_schema <- function(parsed) {
  req_properties <- tibble::tibble(
    name = names(parsed),
    value = unname(parsed)
  ) |>
    dplyr::filter(name != "properties")

  col_properties <- tibble::tibble(
    name = names(parsed$properties),
    value = purrr::map_chr(parsed$properties, 1)
  )

  list(req_properties, col_properties)
}

to_tbl_data <- function(parsed) {
  parsed |>
    mm_keep_df() |>
    tibble::as_tibble()
}

to_tbl_criteria <- function(parsed) {
  tibble::tibble(
    name = names(parsed),
    value = unlist(parsed)
  )
}
