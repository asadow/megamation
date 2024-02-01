#' Are there credentials on hand?
#'
#' @family low-level API functions
#' @keywords internal
#' @returns `TRUE` if user has `MEGAMATION_KEY` and `MEGAMATION_URL` environment
#' variables.
has_creds <- function() {
  key <- Sys.getenv("MEGAMATION_KEY")
  url <- Sys.getenv("MEGAMATION_URL")
  !identical(key, "") && !identical(url, "")
}

# Checking and re-formatting URL
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

# Are httr2 parameters well-specified
check_params <- function(x, call = rlang::caller_env()) {
  np <- names(x)
  if (any(grepl("key", np))) {
    keys <- np[grep("key", np)]
    cli::cli_abort(c(
      "Prevented filter {.arg keys} from being included in the request URL.",
      "i" = "Use environment variables for credentials instead."
      ))
  }

  if (any(startsWith(np, "."))) {
    dotted <- np[grep("\\.", np)]
    cli::cli_abort(c(
      "Prevented filter {.arg dotted} from being included in the request URL.",
      "i" = 'Did you mean `{sub("\\\\.", "", dotted[1])} = "<value>"`?'
    ))
  }
  return()
}

# Is httr2_request
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
