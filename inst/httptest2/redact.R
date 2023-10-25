headers <- c("Cache-Control", "Server", "X-Powered-By",
             "Access-Control-Allow-Origin", "WWW-Authenticate", "X-Powered-By",
             "X-Robots-Tag", "Date", "Content-Length", "Set-Cookie",
             "Strict-Transport-Security")

function(response) {
  response |>
    gsub_response("https://api.megamation.com/uog/dl/", "", fixed = TRUE) |>
    gsub_response("(EMPLOYEE|EMP).*", "") |>
    redact_headers(headers)
}