test_that("remove_api_urls() removes the '_links' field", {
  df <- tibble::tibble("_links" = 1:3, .name_repair = "minimal")
  expect_equal(remove_api_urls(df), df[-1])
})
