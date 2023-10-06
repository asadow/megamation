#' Extract data in the JSON list returned by Megamation's API
#' @param list_from_json A list returned by `jsonlite::fromJSON()`.
#' @export

mm_pluck_data <- function(list_from_json) {
  list_from_json |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1) |>
    tibble::as_tibble()
}
