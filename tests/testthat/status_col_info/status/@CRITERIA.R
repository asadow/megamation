structure(list(method = "GET", url = "status/@CRITERIA", status_code = 200L, 
    headers = structure(list(`Cache-Control` = "REDACTED", `Content-Type` = "application/hal+json", 
        Server = "REDACTED", `X-Powered-By` = "REDACTED", `Access-Control-Allow-Origin` = "REDACTED", 
        `WWW-Authenticate` = "REDACTED", `X-Powered-By` = "REDACTED", 
        `X-Robots-Tag` = "REDACTED", Date = "REDACTED", `Content-Length` = "REDACTED", 
        `Set-Cookie` = "REDACTED", `Strict-Transport-Security` = "REDACTED"), class = "httr2_headers"), 
    body = as.raw(c(0x7b, 0x22, 0x54, 0x61, 0x62, 0x6c, 0x65, 
    0x22, 0x3a, 0x22, 0x53, 0x54, 0x41, 0x54, 0x55, 0x53, 0x5f, 
    0x43, 0x4f, 0x44, 0x45, 0x5f, 0x4d, 0x41, 0x53, 0x54, 0x45, 
    0x52, 0x22, 0x2c, 0x22, 0x43, 0x72, 0x69, 0x74, 0x65, 0x72, 
    0x69, 0x61, 0x22, 0x3a, 0x22, 0x43, 0x6f, 0x6c, 0x75, 0x6d, 
    0x6e, 0x73, 0x20, 0x41, 0x76, 0x61, 0x69, 0x6c, 0x61, 0x62, 
    0x6c, 0x65, 0x20, 0x74, 0x6f, 0x20, 0x73, 0x65, 0x61, 0x72, 
    0x63, 0x68, 0x2f, 0x66, 0x69, 0x6c, 0x74, 0x65, 0x72, 0x20, 
    0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x73, 0x22, 0x2c, 0x22, 
    0x41, 0x4c, 0x4c, 0x46, 0x49, 0x45, 0x4c, 0x44, 0x53, 0x22, 
    0x3a, 0x22, 0x53, 0x65, 0x74, 0x20, 0x74, 0x6f, 0x20, 0x31, 
    0x20, 0x74, 0x6f, 0x20, 0x73, 0x65, 0x65, 0x20, 0x61, 0x6c, 
    0x6c, 0x20, 0x61, 0x76, 0x61, 0x69, 0x6c, 0x61, 0x62, 0x6c, 
    0x65, 0x20, 0x66, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x2e, 0x20, 
    0x20, 0x55, 0x73, 0x65, 0x66, 0x75, 0x6c, 0x20, 0x66, 0x6f, 
    0x72, 0x20, 0x72, 0x65, 0x70, 0x6f, 0x72, 0x74, 0x69, 0x6e, 
    0x67, 0x22, 0x7d, 0x20, 0x0d, 0x0a)), request = structure(list(
        url = "https://api.megamation.com/uog/dl/status/@CRITERIA", 
        method = NULL, headers = structure(list(Authorization = "REDACTED"), redact = "Authorization"), 
        body = NULL, fields = list(), options = list(useragent = "megamation (https://github.com/asadow/megamation)"), 
        policies = list(error_body = function (resp) 
        {
            if (!httr2::resp_has_body(resp)) {
                return("No response body.")
            }
            purrr::pluck(httr2::resp_body_json(resp), "detail")
        }, cache_path = "C:\\Users\\asadowsk\\AppData\\Local\\Temp\\RtmpgBrDim", 
            cache_use_on_error = FALSE, cache_debug = TRUE, cache_max = list(
                age = Inf, n = Inf, size = 1073741824))), class = "httr2_request"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
