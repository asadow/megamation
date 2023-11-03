with_mock_dir("status_resp", {
  test_that("mm_resp_parse() for data gives list with '_embedded'", {
    skip_on_cran()
    resp <- mm_request("status") |> httr2::req_perform()
    parsed <- resp |> mm_resp_parse()
    expect_type(parsed, "list")
    expect_contains(names(parsed), "_embedded")
  })
})

with_mock_dir("mm_resp_extract-schema", {
  test_that("parsed_extract() returns embedded data", {
    skip_on_cran()
    .from <- "schema"
    parsed <- status_get(.from) |> mm_resp_parse()
    expected_df <- extract_schema(parsed)

    df <- parsed_extract(parsed, .from)
    expect_equal(df, expected_df)
  })
})

test_that("mm_n_pages() finds number of pages", {
  parsed <- NULL
  parsed$page_count <- "54_3"
  expect_equal(mm_n_pages(parsed), 3)
})
