test_that("remove_api_urls() removes the '_links' field", {
  df <- tibble::tibble("_links" = 1:3, .name_repair = "minimal")
  expect_equal(remove_api_urls(df), df[-1])
})

test_that("check_bool() errors on TRUE string", {
  allfields <- "TRUE"
  expect_error(check_bool(allfields), "`allfields` must be")
})

test_that("check_params() errors on dot prefix", {
  params <- list(.allfields = TRUE)
  expect_error(check_params(params), "Prevented filter")
})

test_that("check_string() returns error message", {
  expect_error(check_string(5), "must be a single string")
})
