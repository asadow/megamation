#' Are httr2 parameters well-specified
#' @keywords internal
check_params <- function(x, call = rlang::caller_env()) {
  np <- names(x)
  if (any(grepl("key", np))) {
    keys <- np[grep("key", np)]
    cli::cli_abort(c(
      "Prevented filter {.arg keys} from being included in the request URL.",
      "i" = "It is highly recommended that you run {.fun mm_set_creds},
    and {.emph do not} supply {.arg .key}.",
      "i" = 'A typo like `kee = <your-secret>`
    will end up in the request URL as a filter.'
    ))
  }

  if (any(startsWith("\\.", np))) {
    dotted <- np[grep("\\.", np)]
    cli::cli_abort(c(
      "Prevented filter {.arg dotted} from being included in the request URL.",
      "i" = 'Did you mean `{sub("\\.", "", dotted[1])} = "<value>"`?'
    ))
  }

  return(x)
}

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
  return()
}
#' Warn when .key is not same as MEGAMATION_KEY
#' @keywords internal
check_key <- function(.key) {
  if(.key != get_env_key()) {
    cli::cli_warn(c(
      "The {.arg .key} you provided is not your
        MEGAMATION_KEY environment variable.",
      "i" = "It is highly recommended that you run {.fun mm_set_creds}, or
      `Sys.setenv('MEGAMATION_KEY' = '<your-key>')`,
        and {.emph do not} supply {.arg .key}.",
      "i" = 'A typo like `kee = <your-secret>`
        will end up in the request URL as a filter.'
    ))
  }
  return()
}

#' Are credentials present
#' @keywords internal
check_creds <- function() {
  creds <- c(
    key = Sys.getenv("MEGAMATION_KEY"),
    url = Sys.getenv("MEGAMATION_URL")
  )

  if(any(creds == "")) {
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

  if (!startsWith(x, base_url)) {
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
  if (!rlang::is_bool(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be
      either {.val {TRUE}} or {.val {FALSE}}, not {x}."
    )
  }
  return()
}

#' Is string (length-1 character)
#' @keywords internal
check_string <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env(),
                         optional = FALSE) {
  if (!rlang::is_string(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single string.",
      call = call
    )
  }
  return()
}

#' Is Date
#' @keywords internal
check_date <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!lubridate::is.Date(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a Date, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  if (length(x) == 0) {
    cli::cli_abort(
      "{.arg {arg}} must not have length 0.",
      call = call
    )
  }

  if (any(is.na(x))) {
    cli::cli_abort(
      "{.arg {arg}} must not contain {.val NA}.",
      call = call
    )
  }
  return()
}
