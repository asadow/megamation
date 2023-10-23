date <- seq(Sys.Date(), Sys.Date() + 1, "day")
trade <- c("DM")
params <- list(date = date, trade = trade)

library(purrr)

# Fix params --------------------------------------------------------------

format_params(date = seq(Sys.Date(), Sys.Date() + 1, "day"),
              trade = c("PCO", "DW", "WE"))
format_params(trade = c("PCO", "DW", "WE"))
format_params(trade = I("[]PCO"))
mm_request("workorder", date = Sys.Date())
mm_request("workorder", trade = "[]PCO")

# idea --------------------------------------------------------------------
# could use symbol helpers, like has(PCO) for []PCO
# between, for <>
#
#
# mm_request <- function(endpoint,
#                        ...,
#                        allfields = TRUE,
#                        .get = "data",
#                        .url = get_env_url(),
#                        .key = get_env_key()) {
#   .get <- rlang::arg_match(.get, c("criteria", "labels", "schema", "data"))
#
#   check_bool(allfields)
#   params <- list(...)
#   np <- names(params)
#   if("date" %in% np) {
#     date <- params[["date"]]
#     check_date(date)
#   }
# }
dates <- seq(Sys.Date(), Sys.Date() + 100, "day")
mm_request("workorder", date = dates)
mm_request("workorder", date = Sys.Date())

if (identical(dates, seq(min(dates), max(dates), "day"))) {
  .min <- min(dates) |> format("%m-%d-%Y")
  .max <- max(dates) |> format("%m-%d-%Y")
  glue::glue("<>{.min},{.max}")
}

