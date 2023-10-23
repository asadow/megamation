#' Is boolean (length-1 logical)
#' @keywords internal
check_bool <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()){
  if(!rlang::is_bool(x)){
    cli::cli_abort(
      "{.arg {arg}} must be
      either {.val {TRUE}} or {.val {FALSE}}, not {x}."
    )
  }
}

#' Is string (length-1 character)
#' @keywords internal
check_string <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env(),
                         optional = FALSE){
  if(optional && is.null(x)){return()}

  if(!rlang::is_string(x)){
    cli::cli_abort(
      "{.arg {arg}} must be a single string.",
      call = call
    )
  }
}

#' Check date-like inputs
#'
#' @param arg Date vector.
#' @keywords internal
#' @return single string date/time formatted in ISO8601
check_date <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()){

  if(is.null(x)){return()}

  if(!lubridate::is.Date(x)){
    cli::cli_abort(
      "{.arg {arg}} must be a Date, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}
