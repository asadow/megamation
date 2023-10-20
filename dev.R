

# Factory -----------------------------------------------------------------

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}


# IDEA: Apply a different data process depending on type of resp ----------

# could have
#
# mm_req_criteria
# mm_req_labels
# mm_req_schema

## OR

req <- "workorder/@SCHEMA" |>
  mm_req()

resp <- req |> httr2::req_perform()

library(stringr)

# resp$url has
url <- "https://api.megamation.com/uog/dl/workorder/@LABELS"
url |> str_extract("@.*")

# based on this value, dispatch different methods after for the body
# after body_parse()

# or, have a mm_resp() function for keeping all response attributes
# but a different parsed attr. based on data or CSoL

# S7 ----------------------------------------------------------------------

library(S7)

## Class -------------------------------------------------------------------

mm_resp <- new_class("mm_resp",
  properties = list(
    # url = class_character,
    body = class_raw,
    paginated = class_logical
  ),
  validator = function(self) {
    if (length(self@paginated) != 1) {
      "@paginated must be length 1"
    }
  }
)

resp_paginated <- new_class("resp_paginated", parent = mm_resp)
data <- new_class("data", parent = mm_resp)
criteria <- new_class("criteria", parent = mm_resp)
schema <- new_class("schema", parent = mm_resp)
labels <- new_class("labels", parent = mm_resp)

# Method ------------------------------------------------------------------

mm_tibble <- new_generic("mm_tibble", "x")

method(mm_tibble, data) <- function(x) {
  mm_keep_df(x) |>
    tibble::as_tibble()
}

method(mm_tibble, criteria) <- function(x) {
  tibble(
    name = names(x),
    value = unlist(x)
  )
}

method(mm_tibble, labels) <- method(mm_tibble, criteria)

method(mm_tibble, schema) <- function(x) {
  req_properties <- tibble(
    name = names(x),
    value = unname(x)
  ) |>
    dplyr::filter(name != "properties")

  col_properties <- tibble(
    name = names(x$properties),
    value = purrr::map_chr(x$properties, 1)
  )

  list(req_properties, col_properties)
}

# Instance of class -------------------------------------------------------

parsed <- resp |> httr2::resp_body_raw() |> body_parse()
schema_resp <- schema(body = parsed, paginated = FALSE)

mm_tibble(schema_resp)






