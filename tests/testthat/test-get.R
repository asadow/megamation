without_internet({
  expect_GET(
    mm_data("workorder") |> suppressMessages(),
    "https://api.megamation.com/uog/dl/workorder?ALLFIELDS=1"
  )
})

with_mock_dir("mm_data", {
  test_that("mm_data() returns a tibble", {
    skip_on_cran()
    got <- mm_data("status")

    expect_s3_class(got, "tbl_df")
    expect_named(got, c("ampc_required", "description", "status", "ts"))
  })
})

with_mock_dir("statuses", {
  test_that("mm_data() returns informative error on invalid endpoint", {
    skip_on_cran()
    expect_message(
      mm_data("statuses"),
      regexp = "This is not a valid web API endpoint."
    )
  })
})

with_mock_dir("trade", {
  test_that("mm_data() returns informative error on no data", {
    skip_on_cran()
    expect_message(
      mm_data("trade", department_code = "f"),
      regexp = "returned errors or no data."
    )
  })
})

with_mock_dir("status_col_info", {
  test_that("mm_names() returns column names and info", {
    skip_on_cran()
    got <- mm_names("status")
    expect_s3_class(got, "tbl_df")
    expect_named(got, c("field", "filter_enabled", "description", "type"))
  })
})

with_mock_dir("status_col_info", {
  test_that("mm_get_criteria() returns field and description columns", {
    skip_on_cran()
    got <- mm_get_criteria("status")
    expect_s3_class(got, "tbl_df")
    expect_named(got, c("field", "description"))
  })
})

with_mock_dir("status_col_info", {
  test_that("mm_get_schema() returns field, description, and type columns", {
    skip_on_cran()
    got <- mm_get_schema("status")
    expect_s3_class(got, "tbl_df")
    expect_named(got, c("field", "description", "type"))
  })
})

with_mock_dir("status_labels", {
  test_that("mm_get_labels() returns field and description columns", {
    skip_on_cran()
    got <- mm_get_labels("status")
    expect_s3_class(got, "tbl_df")
    expect_named(got, c("field", "description"))
  })
})
