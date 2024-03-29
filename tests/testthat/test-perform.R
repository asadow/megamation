with_mock_dir("status_resp", {
  test_that("mm_req_perform() returns list with HTTP response", {
    skip_on_cran()
    resp <- mm_req("status") |> mm_req_perform()
    expect_type(resp, "list")
    expect_s3_class(resp[[1]], "httr2_response")
  })
})
