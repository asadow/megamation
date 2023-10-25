test_that("mm_error_body() extracts detail", {
  msg <- "Hello World"
  resp <- httr2::response_json(body = list(detail = msg))
  expect_equal(mm_error_body(resp), "Hello World")
})

test_that("mm_error_body() informs when response has no body", {
  resp <- httr2::response()
  expect_equal(mm_error_body(resp), "No response body.")
})
