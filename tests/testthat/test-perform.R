with_mock_dir("status_resp", {
  test_that("mm_req_perform() returns list with HTTP response", {
    skip_on_cran()
    resp <- mm_req("status") |> mm_req_perform(max_reqs = 1)
    expect_type(resp, "list")
    expect_s3_class(resp[[1]], "httr2_response")
  })
})

test_that("mm_req_perform() performs Inf requests by default", {
  expect_equal(formals(mm_req_perform)$max_reqs, Inf)
})

with_mock_api({
  test_that("mm_req_perform() returns API page_count = 21 responses", {
    responses <- mm_req("timecard") |> mm_req_perform()
    expect_length(responses, 21)
  })
})
