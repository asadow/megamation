#' Process a Megamation API response
#' @param resp A Meagmation API response.
#' @export

mm_resp_process <- function(resp) {

  paginated_resp <- class(resp) != "httr2_response"


# Insert OOP here for pagination? -----------------------------------------

  if(!paginated_resp) {

# Insert OOP for data, criteria, schema, ... --------------------------------------------------------------

    resp |>
      mm_resp_body() |>
      mm_tibble()
    # convert_to_tibble()

# -------------------------------------------------------------------------

  } else {
    resp |>
      map(
        \(x) x |>
          mm_resp_body() |>
          mm_keep_df(x)
        ) |>
      bind_pages()
    # bind_to_tibble()
  }

}

