with_mock_dir("status", {
  test_that("parse() gives list with embedded data", {
    resp <- mm_request("status") |> httr2::req_perform()
    parsed <- resp |> mm_resp_parse()
    expect_true("_embedded" %in% names(parsed))

    df <- parsed_extract(parsed)
    expect_s3_class(df, "data.frame")

    parsed <- resp |> mm_resp_parse()
    expect_contains(names(parsed), "_embedded")
  })
})

with_mock_dir("status", {
  test_that("mm_req_perform() returns list with HTTP response", {
    resp <- mm_request("status") |> mm_req_perform()
    expect_type(resp, "list")
    expect_s3_class(resp[[1]], "httr2_response")
  })
})

with_mock_dir("status_schema", {
  test_that("parse() gives list with embedded data", {
    resp <- mm_request("status", opts = req_opts(.get = "criteria")) |>
      httr2::req_perform()
    parsed <- resp |> mm_resp_parse()
    df <- extract_schema(parsed)

    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description", "type"))

    df2 <- parsed_extract(parsed, .get = "schema")
    expect_equal(df, df2)
  })
})

with_mock_dir("status_criteria-or-labels", {
  test_that("extract_criteria() gives correct names", {
    resp <- mm_request("status", opts = req_opts(.get = "criteria")) |>
      httr2::req_perform()
    parsed <- resp |> mm_resp_parse()
    df <- extract_criteria(parsed)

    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description"))

    df2 <- parsed_extract(parsed, .get = "criteria")
    expect_equal(df, df2)
  })
})

with_mock_dir("status_criteria-or-labels", {
  test_that("extract_criteria() gives correct names", {
    df <- status_df_for("labels")

    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description"))
  })
})



