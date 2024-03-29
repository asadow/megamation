headers <- c("Cache-Control", "Server", "X-Powered-By",
             "Access-Control-Allow-Origin", "WWW-Authenticate", "X-Powered-By",
             "X-Robots-Tag", "Content-Length", "Set-Cookie",
             "Strict-Transport-Security")

function(response) {
  response <- response |>
    ## To reduce nested folders where mocks are placed
    gsub_response("https://api.megamation.com/uog/dl/", "", fixed = TRUE) |>
    redact_headers(headers) |>
    redact_cookies()
  response$request <- "REDACTED"
  response
}

