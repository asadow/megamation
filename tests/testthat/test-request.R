test_that("mm_request() returns class httr2_request", {
  skip_on_cran()
  req <- mm_request("status")
  expect_s3_class(req, "httr2_request")
  expected_names <- c(
    "url", "method", "headers", "body",
    "fields", "options", "policies"
  )
  expect_named(req, expected_names)
  expected_names <- c(
    "error_body", "cache_path", "cache_use_on_error",
    "cache_debug", "cache_max"
  )
  expect_named(req$policies, expected_names)
})
