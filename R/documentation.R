# Functions to create roxygen2 documention

table_description <- function(keyword) {
  str <- stringr::str_glue("Inspect or change {keyword}.")

  if (keyword %in% mandatory_keywords()) {
    str <-
      stringr::str_glue(
        "{str} {keyword} cannot be removed because it is a mandatory keyword."
      )
  }

  str
}

table_param_value_ending <- function(keyword) {
  if (keyword %in% mandatory_keywords()) {
    stringr::str_glue(
      "If NULL, an error is thrown because {keyword} cannot be removed."
    )
  } else {
    stringr::str_glue("If NULL, {keyword} is removed.")
  }
}

table1_param_value <- function(keyword) {
  start <- stringr::str_glue("Optional. A character string. If missing, the current {keyword} is returned.")

  stringr::str_glue("{start} {table_param_value_ending(keyword)}")
}

table2_param_value <- function(keyword) {
  start <- stringr::str_glue(
    "Optional. A character string to set the value for all languages or a data ",
    "frame with columns 'language' and 'value' to set it for specific languages. ",
    "If 'value' is missing, the current CONTENTS is returned."
    )

  stringr::str_glue("{start} {table_param_value_ending(keyword)}")
}
