test_that("mm_request() returns filter type errors", {
  expect_error(
    mm_request("status", allfields = "TRUE"),
    "`allfields` must be"
  )

  expect_error(
    mm_request("status", .get = 1),
    "`.get` must be"
  )

  expect_error(
    mm_request("status", date = 1),
    "`date` must be"
  )
})

test_that("mm_request() returns class httr2_request", {
  req <- mm_request("status")
  expect_s3_class(req, "httr2_request")
  req_names <- c("url", "method", "headers", "body",
                 "fields", "options", "policies")
  expect_named(req, req_names)
  expect_true(grepl("ALLFIELDS=1", req$url))

  policies <- c("error_body", "cache_path", "cache_use_on_error",
                "cache_debug",
                "cache_max", "paginate")
  expect_named(req$policies, policies)
})

test_that("mm_request() errors with key = ", {
  expect_error(
    mm_request(key = "my-secret"),
    "Prevented"
  )

  expect_error(
    mm_request("status", .keys = "my-secret"),
    "Prevented"
  )

  expect_warning(
    mm_request("status", .key = "my-secret"),
    "not your MEGAMATION_KEY"
  )
})
