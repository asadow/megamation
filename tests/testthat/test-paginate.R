test_that("mm_req_paginate() returns paginated httr2_request", {
  req <- httr2::request("some_url") |> mm_req_paginate()

  expect_error(mm_req_paginate("string"), "must be.*httr2")
  expect_s3_class(req, "httr2_request")
  expect_named(req$policies, "paginate")
})

test_that("mm_req_paginate_custom() returns paginated httr2_request", {
  req <- httr2::request("some_url") |> mm_req_paginate_custom()

  expect_s3_class(req, "httr2_request")
  expect_named(req$policies, "paginate")
})

test_that("mm_bind_then_tbl() binds and unnests cols with values length 1", {

  mtcars_list_cols <- mtcars |>
    dplyr::mutate(
      mpg = as.list(mpg),
      cyl = list(cyl)
      )
  dfs <- list(mtcars, mtcars_list_cols)

  df <- mm_bind_then_tbl(dfs)
  expect_s3_class(df, "tbl")
  expect_type(df$mpg, "double")
  expect_type(df$cyl, "list")
})
