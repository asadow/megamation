status_df_for <- function(.get) {
  resp <- mm_request("status", .get = "criteria") |> httr2::req_perform()
  parsed <- resp$body |> body_parse()
  df <- to_tbl_criteria(parsed)
}
