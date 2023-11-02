test_that("req_opts() warns when key is provided", {
  expect_warning(
    req_opts(.key = "my-secret"),
    "not your MEGAMATION_KEY environment variable"
  )
})
