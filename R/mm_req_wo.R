#' Extract Megamation workorders
#' @param url A URL starting with "https://api.megamation.com/".
#' @param status Work order status. For example, S.
#' @param type Work order type.
#' @param date Work order date.
#' @export

mm_req_workorder <- function(url, status = NULL, type = NULL, date) {
  if (!is.null(status)) {
    status <- match.arg(status, c("S", "???"))
  }



}
