# Functions to create roxygen2 documention

add_documentation_table1 <- function(keyword) {
  c(paste0("@title ", keyword),
    "",
    paste0("@description ", table_description(keyword)),
    "",
    "@param x A px object",
    paste0("@param value ", table1_param_value(keyword)),
    "@return A px object or a character string",
    "",
    "@export"
  )
}

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

cells_param_value <- function(keyword, number) {
  colname <- tolower(keyword)

  if (number == "1") {
    optional_columns <- "'variable-code', and 'code'"
  } else if (number == "2") {
    optional_columns <- "'variable-code', 'code', and 'language'"
  } else {
    unexpected_error()
  }

  stringr::str_glue(
    "Optional. A data frame with the columns '{colname}' and one or more of the ",
    "columns: {optional_columns}. If 'value' is missing, the current {keyword} ",
    "is returned. If NULL, {keyword} is removed."
  )
}

variables2_param_value <- function(keyword) {
  colname <- tolower(keyword)

  stringr::str_glue(
    "Optional. A character string or data frame.
    \\itemize{{
      \\item Use character to set {keyword} for all languages and variables.
      \\item Use data frame with columns 'variable-code', 'language' and
      '{colname}' to set {keyword} for specific variables.
      \\item If missing, the current {keyword} is returned.
      \\item If NULL, {keyword} is removed for all variables.
    }}
    "
  )
}

acrosscells_param_value <- function(keyword) {
  colname <- tolower(keyword)

  stringr::str_glue(
    "Optional. A data frame with columns '{colname}' and one or more columns ",
    "with the names of the STUB and HEADING variables. The '{colname}' column is ",
    "the {colname} text, and the STUB/HEADING columns control which cells the ",
    "note applies to. Use '*' if the note applies to all cells for a variable.",
    "If 'value' is missing, the current {keyword} is returned. If NULL, {keyword} ",
    "is removed."
  )
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

