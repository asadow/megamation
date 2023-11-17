test_that("absence of API key or URL raises an error", {
  withr::local_envvar(
    c("MEGAMATION_KEY" = "", "MEGAMATION_URL" = "")
  )
  expect_error(
    check_creds(),
    "Megamation API key and/or URL need registering:"
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

test_that("mm_authorize() gives bad url error", {
  expect_error(
    mm_authorize(key = "1", url = "a"),
    "`url` must be of the form"
  )
})

test_that("mm_authorize() sets credentials", {
  withr::defer({
    mm_authorize(
      key = testing_key(),
      url = "https://api.megamation.com/uog/dl",
      overwrite = TRUE
    )
  })
  mm_authorize(
    key = "1",
    url = "https://api.megamation.com/uw/joe/",
    overwrite = TRUE
  )
  expect_false(
    endsWith(Sys.getenv("MEGAMATION_URL"), "/")
  )
  expect_equal(
    mm_url(),
    "https://api.megamation.com/uw/joe"
  )
  expect_equal(
    mm_key(),
    "1"
  )
})

test_that("presence of bad creds raises an error", {
  withr::local_envvar(
    c(
      "MEGAMATION_KEY" = "2",
      "MEGAMATION_URL" = "https://api.megamation.com/uw/bob/"
    )
  )
  expect_error(
    mm_authorize(
      key = "1",
      url = "https://api.megamation.com/uw/joe/"
    ),
    "Megamation credentials already exist"
  )
})