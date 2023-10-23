#' Format a list of parameters
#'
#' @description
#' `format_params()` formats supplied name-value pairs toward
#' creating a valid Megamation URL.
#' @inheritParams mm_request
#' @returns A list of parameter name-value pairs.
#' @export
format_params <- function(...) {
  params <- list(...)
  np <- names(params)
  if ("date" %in% np) {
    date <- params[["date"]]
    date |> check_date()

    .min <- min(date)
    .max <- max(date)
    date_is_sequence <- identical(date, seq(.min, .max, "day")) && .min != .max

    params[["date"]] <- if (!date_is_sequence) {
      params[["date"]] |> format("%m-%d-%Y")
      } else {
      .min <- .min |> format("%m-%d-%Y")
      .max <- .max |> format("%m-%d-%Y")
      glue::glue("<>{.min},{.max}")
    }
  }

  names(params) <- toupper(np)

  max_length <- purrr::map_dbl(params, length) |> max()

  valid_list <- 1:max_length |>
    purrr::map(\(x) map(params, x)) |>
    purrr::flatten() |>
    purrr::compact() |>
    purrr::map(as.character) |>
    purrr::map(I)

  valid_list[order(names(valid_list))]
}
