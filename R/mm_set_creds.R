#' Install Megamation credentials in your `.Renviron` file for repeated use
#'
#' @description This function adds your Megamation API key and base URL to your
#' `.Renviron` file so it can be called securely without being stored in
#' your code. After you have installed these two credentials, they can be
#' called any time with `Sys.getenv("MEGAMATION_KEY")` or
#' `Sys.getenv("MEGAMATION_URL")`. If you do not have an
#' `.Renviron` file, the function will create one for you. If you already
#' have an `.Renviron` file, the function will append the key to your
#' existing file, while making a backup of your original file for disaster
#' recovery purposes.
#' @param key The API key provided to you by Megamation formatted in quotes.
#' @param url The API URL provided to you by Megamation
#' formatted in quotes.
#' @param install If TRUE, will install the key in your `.Renviron` file
#' for use in future sessions.  Defaults to FALSE (single session use).
#' @param overwrite If TRUE, will overwrite existing Megamation
#' credentials that you already have in your `.Renviron` file.
#' @param report If TRUE, ignores other arguments and outputs credentials as a
#' 2-element named vector.
#' @examples
#'
#' \dontrun{
#' mm_set_creds(
#'   key = "<YOUR-MEGAMATION_KEY>",
#'   url = "<YOUR-MEGAMATION_URL>",
#'   install = TRUE
#' )
#' # Reload your environment so you can use the credentials without restarting R
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("MEGAMATION_KEY")
#'
#' # If you need to overwrite existing credentials:
#' mm_set_creds(
#'   key = "<YOUR-MEGAMATION_KEY>",
#'   url = "<YOUR-MEGAMATION_URL>",
#'   install = TRUE,
#'   overwrite = TRUE,
#'   install = TRUE
#' )
#' # Reload your environment to use the credentials
#' }
#' @export

mm_set_creds <- function(key,
                         url,
                         overwrite = FALSE,
                         install = FALSE,
                         report = FALSE) {

    checkarg_isboolean(report)

    if(report){
      creds <- c(
        key = Sys.getenv("MEGAMATION_KEY"),
        url = Sys.getenv("MEGAMATION_URL")
      )
      return(creds)
    }

    checkarg_isboolean(overwrite)
    checkarg_isboolean(install)
    checkarg_isstring(key)

    if (install) {

      home <-
        Sys.getenv("HOME")
      renv <-
        file.path(home, ".Renviron")

      # If needed, backup original .Renviron before doing anything else here.
      if (file.exists(renv)) {
        file.copy(renv, file.path(home, ".Renviron_backup"))
      }

      if (!file.exists(renv)) {
        file.create(renv)
      } else {
        if (isTRUE(overwrite)) {
          message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
          oldenv <- readLines(renv)
          newenv <- oldenv[-grep("MEGAMATION_KEY|MEGAMATION_URL", oldenv)]
          writeLines(newenv, renv)
        }
        else {
          tv <- readLines(renv)
          if (any(grepl("MEGAMATION_KEY|MEGAMATION_URL", tv))) {
            stop("Megamation credentials already exist. You can overwrite them with the argument overwrite=TRUE", call. = FALSE)
          }
        }
      }

      keyconcat <- paste0("MEGAMATION_KEY = '", key, "'")
      urlconcat <- paste0("MEGAMATION_URL = '", url, "'")
      # Append credentials to .Renviron file
      write(keyconcat, renv, sep = "\n", append = TRUE)
      write(urlconcat, renv, sep = "\n", append = TRUE)
      message('Your Megamation key and URL have been stored in your .Renviron.  \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    } else {
      message("To install your credentials for use in future sessions, run this function with `install = TRUE`.")
      Sys.setenv(
        MEGAMATION_KEY = key,
        MEGAMATION_URL = url
      )
    }
  }
