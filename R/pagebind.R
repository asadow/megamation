#' Bind multiple Megamation API pages by row before converting to a tibble
#'
#' `mm_pagebind()` is needed as pages can have same-named fields with different
#' types. This is because some field(s) of a given page may or may not contain
#' vectors  of values in one of its rows. `mm_pagebind()` takes care of this
#' possibility by treating each page as a matrix before binding and unnesting
#' their combinations.
#'
#' @param pages List of data frames representing Megamation API pages.
#' @returns A data frame of class [`tbl_df`][tibble::tbl_df-class] representing
#'   the bound pages.
#' @keywords internal
#' @examples
#' page_1 <- mtcars |>
#'   dplyr::mutate(
#'     mpg = as.list(mpg),
#'   )
#' page_2 <- page_1 |>
#'   dplyr::mutate(
#'     cyl = list(cyl)
#'   )
#' pages <- list(page_1, page_2)
#' # mpg unnests but not cyl
#' megamation:::mm_pagebind(pages)
mm_pagebind <- function(pages) {
  pages <- purrr::compact(pages)
  matrices <- purrr::map(pages, \(x) as.matrix(x))
  m <- purrr::reduce(matrices, rbind, .init = tibble::tibble())
  data <- m |> tibble::as_tibble()

  cols <- names(data)
  lengths <- purrr::map_dbl(
    cols,
    \(x) purrr::map_dbl(data[[x]], length) |> max()
  )
  unlisted <- cols[lengths %in% 0:1]

  data |> tidyr::unnest(!!unlisted)
}
