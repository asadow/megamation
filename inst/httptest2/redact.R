headers <- c("Cache-Control", "Server", "X-Powered-By",
             "Access-Control-Allow-Origin", "WWW-Authenticate", "X-Powered-By",
             "X-Robots-Tag", "Date", "Content-Length", "Set-Cookie",
             "Strict-Transport-Security")

function(response) {
  response <- response |>
    redact_headers(headers) |>
    redact_cookies()
  response$request <- "REDACTED"
  response
}

