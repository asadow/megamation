with_mock_dir("status_resp", {
  test_that("mm_next_req() returns NULL where one page exists", {
    skip_on_cran()
    req <- mm_request("status")
    resp <- httr2::req_perform(req)
    result <- mm_next_req(resp, req)
    expect_null(mm_next_req(resp, req))
  })
})
