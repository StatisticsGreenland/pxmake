# Functions to create roxygen2 documention

description_start <- function(keyword) {
  stringr::str_glue("Inspect or change {keyword}.")
}

table_description <- function(keyword) {
  str <- description_start(keyword)

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
    "If 'value' is missing, the current {keyword} is returned."
    )

  stringr::str_glue("{start} {table_param_value_ending(keyword)}")
}

note_description <- function(keyword) {
  stringr::str_glue("{description_start(keyword)}. {keyword} can be set for the ",
                    "entire table or for a specific variable."
                    )
}

note_param_value <- function(keyword) {
  colname <- tolower(keyword)
  stringr::str_glue(
    "
    Optional. A character string, a data frame, or a list.
     \\itemize{{
       \\item Use character, to set {keyword} for the entire table across all languages.
       \\item Use a data frame with columns 'language' and 'value' to set
       {keyword} for the entire table in a specific language.
       \\item Use a data frame with the columns 'variable-code' and '{colname}',
       to set {keyword} for a specific variable across all languages. Add the
       column 'language' to set {keyword} for specific language.
       \\item Use a list of the above elements to set {keyword} in muliple ways.
       This is the same as calling {keyword} multiple times with different values.
       \\item If missing, the current {keyword} is returned.
       \\item If NULL, {keyword} is removed for the table and all variables.
    }}
    "
  )
}

pivot_param_variables <- function(keyword) {
  stringr::str_glue(
    "
    Optional. A character vector of variable names to change to
    STUB. This also changes the {keyword} order. With names in `variables`
    becoming 1, 2, ... If missing, the current {keyword} variables are returned.
    "
  )
}

