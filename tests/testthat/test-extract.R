with_mock_dir("mm_resp_extract-schema", {
  skip_on_cran()
  test_that("mm_resp_extract() for schema returns embedded data", {
    df <- status_get("schema") |> mm_resp_extract()
    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description", "type"))
  })
})

with_mock_dir("mm_resp_extract-criteria", {
  skip_on_cran()
  test_that("mm_resp_extract() for criteria returns embedded data", {
    df <- status_get("criteria") |> mm_resp_extract()
    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description"))
  })
})
