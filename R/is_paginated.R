#' Detect whether a request is paginated
#' @param x An API request.
#' @export
is_paginated <- function(x) {
  # class(x) != "httr2_response"
  # class(x) = "httr2_response"


  # if(class(x) == "character")
  #   x == "data"
  #
  # if(class(x) == "httr2_response")



  if(class(x) == "httr2_request")
    "paginate" %in% names(x$policies)

}
