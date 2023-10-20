#' Bind multiple Megamation API pages by row before converting to a tibble
#' @param pages List of data frames representing Megamation API pages.
#' @export
mm_bind_then_tbl <- function(pages) {
  matrices <- map(pages, \(x) as.matrix(x))
  m <- reduce(matrices, rbind)
  data <- m |> tibble::as_tibble()

  cols <- names(data)
  lengths <- purrr::map_dbl(
    cols,
    \(x) purrr::map_dbl(data[[x]], length) |> max()
    )
  unlisted <- cols[lengths %in% 0:1]

  data |> tidyr::unnest(!!unlisted)
}

## Or, if using req_paginate_custom()
# matrices <- map(pages, \(x) pluck(x, "_embedded", "WorkOrder") |> as.matrix())
