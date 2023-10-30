status_df_for <- function(.get) {
  resp <- mm_request("status", opts = req_opts(.get = "criteria")) |>
    httr2::req_perform()
  parsed <- resp |> mm_resp_parse()
  df <- extract_criteria(parsed)
}
