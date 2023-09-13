#' @export

find_data <- function(ls) {
  ls |>
    purrr::list_flatten() |>
    purrr::keep(\(x) is.data.frame(x)) |>
    purrr::pluck(1) |>
    tibble::as_tibble()
}
