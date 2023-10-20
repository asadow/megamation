#' Is boolean (length-1 logical)
#' @keywords internal
check_bool <- function(x, arg = caller_arg(x), call = caller_env()){
  if(!rlang::is_bool(x)){
    cli::cli_abort(
      "{.arg {arg}} must be
      either {.val {TRUE}} or {.val {FALSE}}, not {x}."
    )
  }
}

#' Is string (length-1 character)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
check_string <- function(x,
                         arg = caller_arg(x),
                         call = caller_env(),
                         optional = FALSE){
  if(optional && is.null(x)){return()}

  if(!rlang::is_string){
    cli::cli_abort(
      "{.arg {arg}} must be a single string.",
      call = call
    )
  }
}

#' Title Check date-like inputs and convert them
#'
#' @param arg Date, POSIX(c/l)t date/time, or parse-able string in
#'   YYYY(/-)MM(/-)DD format.
#'   Intended to be converted to MM-DD-YYYY format.
#' @importFrom lubridate is.POSIXt
#' @importFrom lubridate is.Date
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom rlang expr_text
#' @keywords internal
#' @return single string date/time formatted in ISO8601
check_date <- function(x, arg = caller_arg(x), call = caller_env()){

  if(is.null(x)){return()}

  # Check that x is correct type:
  test_x_class <- lubridate::is.POSIXt(x) | lubridate::is.Date(x) |
    is.character(x)

  if(!test_x_class){
    cli::cli_abort(
      "{.arg {arg}} must be a string, Date, POSIXlt, or POSIXct,
      not {.obj_type_friendly {x}}.",
      call = call
    )
  }

 if(!length(x) == 1){
   cli::cli_abort(
     "{.arg {arg}} must have length 1,
      not {length(x)}.",
     call = call
   )
 }

  # Check that x is correct format:
  if(is.character(x)){
    date_format_pattern <-
      glue::glue(
        "^[0-9]{{4}}",
        "(0[1-9]|1[0-2])",
        "([0-2][0-9]|3[0-1])$",
        .sep = "[-/]"
      )

    test_date_format <- stringr::str_detect(x, date_format_pattern)
    if(!test_date_format){
      cli::cli_abort(
        "{.arg {arg}} must have format
        YYYY/MM/DD or YYYY-MM-DD. Format of {x} is invalid.",
        call = call
      )
    }
  }

  # Attempt to parse the date object:
  date_parsed <- clock::date_parse(x)

  # If it didn't parse for some other reason, throw an error:
  if(is.na(date_parsed)){
    cli::cli_abort(c(
      "{.arg {arg}} failed to parse. Reason unknown."
    ))
  }
  # Format in appropriate form for API call:
  date_formatted <- format(date_parsed, "%m-%d-%Y")

  return(date_formatted)
}
