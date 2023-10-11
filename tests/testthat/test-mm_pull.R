test_that("mm_pull() returns a tibble", {
  resp <- mm_pull("status")
  expect_s3_class(resp, "tbl_df")
})
