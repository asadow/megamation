#' Is boolean (length-1 logical)
#' @keywords internal
checkarg_isboolean <- function(arg){
  test <- is.logical(arg) && !is.na(arg) && length(arg) == 1

  if(!test){
    cli::cli_abort(
      c("Error in argument '{deparse(substitute(arg))}':",
        "Argument must be a single `TRUE` or `FALSE`.")
    )
  }
}

#' Is string (length-1 character)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isstring <- function(arg, null_okay = TRUE){
  if(null_okay && is.null(arg)){return()}

  test <- is.character(arg) && length(arg) == 1

  if(!test){
    cli::cli_abort(
      c("Error in argument '{deparse(substitute(arg))}':",
        "Argument must be a single string.")
    )
  }
}
