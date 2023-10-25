test_that("format_params() formats a sequence of dates", {
  date_start <- as.Date("2023-09-20")
  date_end <- as.Date("2023-10-20")
  date <- seq(date_start, date_end, by = "day")

  expected <- list(DATE = structure("<>09-20-2023,10-20-2023", class = "AsIs"))
  expect_equal(format_params(date = date), expected)
})

test_that("format_params() formats dates not in sequence", {
  date <- as.Date("2023-09-20")
  date <- c(date, date + 3)

  expected <- list(
    DATE = structure("09-20-2023", class = "AsIs"),
    DATE = structure("09-23-2023", class = "AsIs")
    )
  expect_equal(format_params(date = date), expected)
})

test_that("format_params() does not return accidental key", {
  expect_error(format_params(key = "secret"), "Prevented")
})
