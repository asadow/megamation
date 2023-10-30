
with_mock_dir("status", {
  test_that("mm_get() returns a tibble", {
    skip_on_cran()
    got <- mm_get("status")

    expect_s3_class(got, "tbl_df")
    expect_named(got, c("ampc_required", "description", "status"))
  })
})

with_mock_dir("status", {
  test_that("mm_req_perform() returns a response", {
    skip_on_cran()

    one_resp <- mm_request("status") |> mm_req_perform()
    expect_s3_class(one_resp[[1]], "httr2_response")

    many_resp <- mm_request("status") |> mm_req_paginate() |> mm_req_perform()
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

with_mock_dir("status_schema", {
  test_that("mm_get() returns correct cols", {
    skip_on_cran()
    got <- mm_get("status", .get = "schema", .paginate = TRUE)
    expect_s3_class(got, "tbl_df")
    expect_named(got, c("field", "description", "type"))
  })
})
