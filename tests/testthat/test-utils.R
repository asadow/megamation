test_that("remove_api_urls() removes the '_links' field", {
  df <- tibble::tibble("_links" = 1:3, .name_repair = "minimal")
  expect_equal(remove_api_urls(df), df[-1])
})

test_that("check_bool() errors on TRUE string", {
  allfields <- "TRUE"
  expect_error(check_bool(allfields), "`allfields` must be")
})

test_that("check_string() returns error message", {
  expect_error(check_string(5), "must be a single string")
})

test_that("absence of API key or URL raises an error", {
  withr::local_envvar("MEGAMATION_URL" = "")
  expect_error(
    mm_url(),
    "Missing credentials."
  )
})

test_that("testing key", {
  skip_on_cran()
  expect_equal(
    mm_key(),
    testing_key()
  )
})

test_that("mm_url() gives bad url error", {
  withr::local_envvar("MEGAMATION_URL" = "a.com")
  expect_error(
    mm_url(),
    "`MEGAMATION_URL` must be of the form"
  )
})
