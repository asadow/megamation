#' Perform a Megamation API request
#'
#' After creating a request with [mm_request()],
#' call `mm_req_perform()` to perform it and fetch
#' the results back to R.
#'
#' @inheritParams mm_req_paginate
#' @returns An [httr2::response] or a list of such responses where the request
#' is paginated.
#' @export
mm_req_perform <- function(req) {

  if(!is_paginated(req)) {
    req |> httr2::req_perform()
  } else {
    req |> httr2::req_perform_iteratively()
  }

}

