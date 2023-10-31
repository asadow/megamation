with_mock_dir("status", {
  test_that("mm_resp_parse() for data gives list with '_embedded'", {
    resp <- mm_request("status") |>
      httr2::req_perform()
    parsed <- resp |> mm_resp_parse()
    expect_type(parsed, "list")
    expect_contains(names(parsed), "_embedded")
  })
})

with_mock_dir("status", {
  test_that("mm_req_perform() returns list with HTTP response", {
    resp <- mm_req("status") |> mm_req_perform()
    expect_type(resp, "list")
    expect_s3_class(resp[[1]], "httr2_response")
  })
})

# with_mock_dir("status_schema", {
#   test_that("parsed_extract() returns embedded data", {
#     .from <- "schema"
#     parsed <- status_get(.from) |> mm_resp_parse()
#     expected_df <- extract_schema(parsed)
#
#     df <- parsed_extract(parsed, .from)
#     expect_equal(df, expected_df)
#   })
# })

with_mock_dir("status_schema", {
  test_that("mm_resp_extract() for schema returns embedded data", {
    df <- status_get("schema") |> mm_resp_extract()
    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description", "type"))
  })
})

with_mock_dir("status_criteria-or-labels", {
  test_that("mm_resp_extract() for criteria returns embedded data", {
    df <- status_get("criteria") |> mm_resp_extract()
    expect_s3_class(df, "data.frame")
    expect_named(df, c("field", "description"))
  })
})


