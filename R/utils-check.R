#' Are there credentials on hand?
#'
#' @family low-level API functions
#' @keywords internal
#' @returns `TRUE` if user has `MEGAMATION_KEY` and `MEGAMATION_URL` environment
#'   variables.
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

  date_index <- grep("date", np, ignore.case = TRUE)
  if (!rlang::is_empty(date_index)) {
    date <- x[[date_index]]
    check_date(date)
  }

  ## bb: bang-bang !!
  x_has_bb_index <- x |>
    purrr::map_lgl(\(y) stringi::stri_detect(y, fixed = "!!") |> any())
  x_has_bb_and_more <- x[x_has_bb_index] |> purrr::map_dbl(length) > 1

  if (any(x_has_bb_and_more)) {

    x_ls <- x[x_has_bb_and_more] |>
      purrr::imap_chr(\(x, idx) glue::glue("`{idx}` has length {length(x)}."))
    names(x_ls) <- rep("x", length(x_ls))

    cli::cli_abort(c(
      "{.code {names(x_has_bb_and_more)}} must have length 1 with use of `!!`.",
      x_ls,
      "i" = "
      {.fun mm_data} performs and binds the results of separate GET requests for
      each value supplied to a parameter (as Megamation's API won't allow
      multiple values in a single request).
      Combining the results of a request that uses `!!` with the results of
      another request can result in duplicate data."
    ))
  }

  key_index <- grep("key", np, ignore.case = TRUE)
  if (!rlang::is_empty(key_index)) {
    keys <- np[[key_index]]
    cli::cli_abort(c(
      "Prevented filter {.code {keys}} from being included in the request URL.",
      "i" = "Use {.fun mm_authorize} or environment variables for credentials
      instead."
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
