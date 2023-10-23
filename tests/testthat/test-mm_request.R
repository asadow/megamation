test_that("mm_request() returns class httr2_request", {
  resp <- mm_request("status")
  expect_s3_class(resp, "httr2_request")
})
