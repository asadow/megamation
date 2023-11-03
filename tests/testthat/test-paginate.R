test_that("mm_req_paginate() returns paginated httr2_request", {
  req <- httr2::request("some_url") |> mm_req_paginate()

  expect_error(mm_req_paginate("string"), "must be.*httr2")
  expect_s3_class(req, "httr2_request")
  expect_named(req$policies, "paginate")
})
