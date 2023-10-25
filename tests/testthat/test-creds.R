test_that("absence of API key or URL raises an error", {
  Sys.setenv("MEGAMATION_KEY" = "")
  Sys.setenv("MEGAMATION_URL" = "")
  Sys.setenv("MEGAMATION_USER" = "")
  expect_error(
    check_creds(),
    "Megamation API key and/or URL need registering:"
  )
  expect_equal(
    get_env_key(),
    testing_key()
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

test_that("can store and access credentials", {
  expect_error(
    mm_set_creds(key = "1", url = "a"),
    "`url` must be of the form"
  )

  mm_set_creds(
    key = "1",
    url = "https://api.megamation.com/uw/joe/",
    overwrite = TRUE
    )

  expect_equal(
    get_env_key(),
    "1"
  )
  expect_false(
    endsWith(Sys.getenv("MEGAMATION_URL"), "/")
  )
  expect_equal(
    get_env_url(),
    "https://api.megamation.com/uw/joe"
  )
  Sys.setenv("MEGAMATION_USER" = "Bob")
  expect_equal(
    get_env_user(),
    "Bob"
  )

  expect_null(
    check_creds()
  )
})

test_that("presence of creds raises an error", {
  Sys.setenv("MEGAMATION_KEY" = "2")
  Sys.setenv("MEGAMATION_URL" = "https://api.megamation.com/uw/bob/")

  expect_error(
    mm_set_creds(
      key = "1",
      url = "https://api.megamation.com/uw/joe/"
    ),
    "Megamation credentials already exist"
  )

})

# Restore creds for other tests:
mm_set_creds(
  key = testing_key(),
  url = 'https://api.megamation.com/uog/dl',
  overwrite = TRUE
  )
