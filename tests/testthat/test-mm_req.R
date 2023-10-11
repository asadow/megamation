test_that("mm_req() returns class httr2_request", {
  resp <- mm_req("status")
  expect_s3_class(resp, "httr2_request")
})
