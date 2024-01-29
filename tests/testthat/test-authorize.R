test_that("absence of API key or URL raises an error", {
  withr::local_envvar("MEGAMATION_URL" = "")
  expect_error(
    mm_url(),
    "Missing credentials"
  )
})

test_that("testing key", {
  skip_on_cran()
  expect_equal(
    mm_key(),
    testing_key()
  )
})

test_that("mm_auth() gives bad url error", {
  expect_error(
    mm_auth(key = "1", url = "a"),
    "`url` must be of the form"
  )
})

test_that("mm_auth() sets credentials", {
  skip_on_cran()
  key_original <- mm_key()
  url_original <- mm_url()
  withr::defer({
    mm_auth(key = key_original, url = url_original)
  })
  mm_auth(key = "1", url = "https://api.megamation.com/uw/joe/")
  expect_equal(
    mm_url(),
    "https://api.megamation.com/uw/joe"
  )
  expect_equal(
    mm_key(),
    "1"
  )
})
