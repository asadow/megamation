#' Is httr2_request
#' @keywords internal
check_request <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!inherits(x, "httr2_request")) {
    cli::cli_abort(
      "{.arg {arg}} must be an {.cls httr2_request},
      not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

#' Are credentials present
#' @keywords internal
check_creds <- function(){
  creds <- c(
    key = Sys.getenv("MEGAMATION_KEY"),
    url = Sys.getenv("MEGAMATION_URL")
  )

  if(any(creds == "")){
    cli::cli_abort(c(
      "Megamation API key and/or URL need registering:",
      i = "Use {.fun mm_set_creds}"
    ))
  }

  check_url(Sys.getenv("MEGAMATION_URL"))

  return()

}

#' Checking and re-formatting URL
#' @keywords internal
check_url <- function(x,
                      arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {

  check_string(x)

  base_url <- "https://api.megamation.com/"

  if (!startsWith(x, base_url)){
    cli::cli_abort(
      "{.arg {arg}} must be of the form {.val {base_url}<institution ID>/dl},
    not {x}."
    )
  }

  if (endsWith(x, "/")) x <- sub("/$", "", x)

  return(x)
}

#' Is boolean (length-1 logical)
#' @keywords internal
check_bool <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
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
                         optional = FALSE) {
  if(optional && is.null(x)){return()}

  if(!rlang::is_string(x)){
    cli::cli_abort(
      "{.arg {arg}} must be a single string.",
      call = call
    )
  }
}

#' Is Date
#' @keywords internal
check_date <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {

  if(is.null(x)){return()}

  if(!lubridate::is.Date(x)){
    cli::cli_abort(
      "{.arg {arg}} must be a Date, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}
