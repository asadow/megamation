test_that("mm_pagebind() binds and unnests cols with values length 1", {
  page_1 <- mtcars |>
    dplyr::mutate(
      mpg = as.list(mpg)
    )
  page_2 <- page_1 |>
    dplyr::mutate(
      cyl = list(cyl)
    )
  page_3 <- NULL

  pages <- list(page_1, page_2, page_3)

  df <- mm_pagebind(pages)
  expect_s3_class(df, "tbl")
  expect_type(df$mpg, "double")
  expect_type(df$cyl, "list")
})
