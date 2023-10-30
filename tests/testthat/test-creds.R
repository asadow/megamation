test_that("absence of API key or URL raises an error", {
  withr::local_envvar(
    c("MEGAMATION_KEY" = "",
      "MEGAMATION_URL" = "",
      "MEGAMATION_USER" = "")
    )
  expect_error(
    check_creds(),
    "Megamation API key and/or URL need registering:"
  )
  expect_error(
    get_env_url(),
    "No `MEGAMATION.* found"
  )
  expect_error(
    get_env_user(),
    "No `MEGAMATION.* found"
  )
})

test_that("testing key", {
  skip_on_cran()
  expect_equal(
    get_env_key(),
    testing_key()
  )
})

test_that("mm_set_creds() gives bad url error", {
  expect_error(
    mm_set_creds(key = "1", url = "a"),
    "`url` must be of the form"
  )
})

test_that("mm_set_creds() sets credentials", {
  mm_set_creds(
    key = "1",
    url = "https://api.megamation.com/uw/joe/",
    overwrite = TRUE
    )
  expect_false(
    endsWith(Sys.getenv("MEGAMATION_URL"), "/")
  )
  expect_equal(
    get_env_url(),
    "https://api.megamation.com/uw/joe"
  )

  skip_on_cran()
  withr::defer({
    mm_set_creds(
      key = testing_key(),
      url = 'https://api.megamation.com/uog/dl',
      overwrite = TRUE
    )
  })
  expect_equal(
    get_env_key(),
    "1"
  )
})

test_that("presence of bad creds raises an error", {
  withr::local_envvar(
    c("MEGAMATION_KEY" = "2",
      "MEGAMATION_URL" = "https://api.megamation.com/uw/bob/",
      "MEGAMATION_USER" = "Bob")
    )
  expect_error(
    mm_set_creds(
      key = "1",
      url = "https://api.megamation.com/uw/joe/"
    ),
    "Megamation credentials already exist"
  )

  # Other tests
  expect_equal(
    get_env_user(),
    "Bob"
  )
  expect_null(check_creds())
})

