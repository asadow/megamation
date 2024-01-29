test_that("format_params() formats a sequence of dates", {
  date_start <- as.Date("2023-09-20")
  date_end <- as.Date("2023-10-20")
  date <- seq(date_start, date_end, by = "day")

  expected <- list(DATE = structure("<>09-20-2023,10-20-2023",
                                    class = c("AsIs", "glue", "character")))
  actual <- format_params(date = date)["DATE"]
  expect_equal(actual, expected)
})

test_that("format_params() formats dates not in sequence", {
  date <- as.Date("2023-09-20")
  date <- c(date, date + 3)

  expected <- list(DATE = structure(c("09-20-2023", "09-23-2023")))
  actual <- format_params(date = date)["DATE"]
  expect_equal(actual, expected)
})

test_that("format_params() does not return accidental key", {
  expect_error(format_params(key = "secret"), "Prevented")
  expect_error(format_params(.keys = "my-secret"), "Prevented")
})

test_that("format_params() returns filter type errors", {
  expect_error(format_params(date = 1), "`date` must be a Date")
})

test_that("format_params() will not accept NA or empty date", {
  expect_error(format_params(date = lubridate::as_date(NA)), "NA")
  expect_error(format_params(date = as.Date(NULL)), "length 0")
})
