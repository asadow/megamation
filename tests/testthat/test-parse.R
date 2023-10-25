with_mock_dir("status", {
  test_that("parse() gives list with embedded data", {
    resp <- mm_request("status") |> httr2::req_perform()
    parsed <- resp$body |> body_parse()
    expect_true("_embedded" %in% names(parsed))

    df <- parsed_to_tbl(parsed)
    expect_s3_class(df, "tbl")

    parsed <- resp |> resp_body_parse()
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
    resp <- mm_request("status", .get = "schema") |> httr2::req_perform()
    parsed <- resp$body |> body_parse()
    df <- to_tbl_schema(parsed)

    expect_s3_class(df, "tbl")
    expect_named(df, c("list_name", "value"))

    df2 <- parsed_to_tbl(parsed, .get = "schema")
    expect_equal(df, df2)
  })
})

with_mock_dir("status_criteria-or-labels", {
  test_that("to_tbl_criteria() gives correct names", {
    resp <- mm_request("status", .get = "criteria") |> httr2::req_perform()
    parsed <- resp$body |> body_parse()
    df <- to_tbl_criteria(parsed)

    expect_s3_class(df, "tbl")
    expect_named(df, c("list_name", "value"))

    df2 <- parsed_to_tbl(parsed, .get = "criteria")
    expect_equal(df, df2)
  })
})

with_mock_dir("status_criteria-or-labels", {
  test_that("to_tbl_criteria() gives correct names", {
    df <- status_df_for("labels")

    expect_s3_class(df, "tbl")
    expect_named(df, c("list_name", "value"))
  })
})



