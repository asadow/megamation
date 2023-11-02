test_that("check_bool() errors on TRUE string", {
  allfields <- "TRUE"
  expect_error(check_bool(allfields), "`allfields` must be")
})
