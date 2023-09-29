#' Find data in the JSON list returned by Megamation's API
#' @param json_list A JSON list.
#' @export

find_data <- function(json_list) {
  json_list |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1) |>
    tibble::as_tibble()
}
