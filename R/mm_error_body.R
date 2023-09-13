#' @export

mm_error_body <- function(resp) {
resp %>% httr2::resp_body_json() %>% .$detail
}
