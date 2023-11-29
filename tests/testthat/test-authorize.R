test_that("absence of API key or URL raises an error", {
  withr::local_envvar(
    c("MEGAMATION_KEY" = "", "MEGAMATION_URL" = "")
  )
  expect_error(
    check_creds(),
    "Missing credentials."
  )
  expect_error(
    mm_url(),
    "No `MEGAMATION.* found"
  )
})

test_that("testing key", {
  skip_on_cran()
  expect_equal(
    mm_key(),
    testing_key()
  )
})

test_that("check_url() gives bad url error", {
  url <- "a"
  expect_error(
    check_url(url),
    "`url` must be of the form"
  )
})
