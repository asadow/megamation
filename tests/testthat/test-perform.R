with_mock_dir("mm_get", {
  test_that("mm_get() returns a tibble", {
    skip_on_cran()
    got <- mm_get("status")

    expect_s3_class(got, "tbl_df")
    expect_named(got, c("ampc_required", "description", "status"))
  })
})

with_mock_dir("status_resp", {
  test_that("mm_req_perform() returns a response", {
    skip_on_cran()
    one_resp <- mm_req("status") |> mm_req_perform()
    expect_s3_class(one_resp[[1]], "httr2_response")

    many_resp <- mm_req("status") |> mm_req_paginate() |> mm_req_perform()
    expect_s3_class(one_resp[[1]], "httr2_response")
  })
})

with_mock_dir("statuses", {
  test_that("mm_get() returns informative error on invalid endpoint", {
    skip_on_cran()
    expect_error(
      mm_get("statuses"),
      regexp = "This is not a valid web API endpoint."
    )
  })
})
