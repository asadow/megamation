test_that("mm_req_params() adds 'ALLFIELDS=1' to URL", {
  req <- httr2::request("some_url") |> mm_req_params()
  expect_true(grepl("ALLFIELDS=1", req$url))
})

test_that("mm_req_append() adds '/@CRITERIA' ending to URL", {
  req <- httr2::request("some_url") |> mm_req_append("criteria")
  expect_true(req$url |> endsWith("/@CRITERIA"))
})
