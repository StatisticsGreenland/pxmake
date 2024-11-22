# Functions to create roxygen2 documention

keyword_to_function <- function(keyword) {
  paste0("px_", gsub("-", "_", tolower(keyword)))
}

function_to_keyword <- function(fnc_name) {
  toupper(substr(gsub("_", "-", fnc_name), 4, nchar(fnc_name)))
}

split_multiline_str_into_vector <- function(str) {
  str %>%
    stringr::str_split(pattern = "\n") %>%
    magrittr::extract2(1)
}

add_documentation_function <- function(fnc) {
  function(...) {
    fnc(...) %>%
      split_multiline_str_into_vector()
  }
}

add_doc_keyword_function_intro <-
  add_documentation_function(doc_keyword_function_intro)

add_documentation_table1 <- function(keyword, example_value) {
  stringr::str_glue(
    "{doc_keyword_function_intro(keyword)}",
    "@param value {table1_param_value(keyword)}",
    param_validate(),
    "{return_px_or_char_str()}",
    "{table1_example(keyword, example_value)}",
    "@export",
    .sep = "\n"
    ) %>%
    split_multiline_str_into_vector()
}

add_documentation_table2 <- function(keyword, example_value1, example_value2) {
  stringr::str_glue(
    "{doc_keyword_function_intro(keyword)}",
    "@param value {table2_param_value(keyword)}",
    param_validate(),
    "{return_px_or_char_vector_or_df()}",
    "{table2_example(keyword, example_value1, example_value2)}",
    "@export",
    .sep = "\n"
    ) %>%
    split_multiline_str_into_vector()
}

add_documentation_head_stub <- function(keyword) {
  stringr::str_glue(
    "{doc_keyword_function_intro(keyword)}",
    "@param value {pivot_param_value(keyword)}",
    param_validate(),
    "{return_px_or_char_vector()}",
    "@export",
    .sep = "\n"
    ) %>%
    split_multiline_str_into_vector()
}

add_documentation_variables2 <- function(keyword, example_value1, example_value2, example_value3) {
  stringr::str_glue(
    "{doc_keyword_function_intro(keyword)}",
    "@param value {variables2_param_value(keyword)}",
    param_validate(),
    "{return_px_or_char_vector_or_df()}",
    "{variables2_example(keyword, example_value1, example_value2, example_value3)}",
    "@export",
    .sep = "\n"
  ) %>%
    split_multiline_str_into_vector()
}

add_documentation_acrosscells <- function(keyword) {
  stringr::str_glue(
    "{doc_keyword_function_intro(keyword)}",
    "@param value {acrosscells_param_value(keyword)}",
    param_validate(),
    "{return_px_or_df()}",
    "{acrosscells_example(keyword)}",
    "@export",
    .sep = "\n"
  ) %>%
    split_multiline_str_into_vector()

}

doc_keyword_function_intro <- function(keyword) {
  str <-
    stringr::str_glue(
    "@title {keyword}",
    "@description Inspect or change {keyword}.",
    "@param x A px object",
    .sep = "\n"
    )

  url <-
    px_keywords %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::pull(documentation)

  if (length(url) == 1) {
    str <-
      stringr::str_glue("{str}",
                      "@seealso [Statistics Sweden's documentation]({url})",
                      .sep = "\n"
                      )
  }
  return(str)
}

description_start <- function(keyword) {
  stringr::str_glue("Inspect or change {keyword}.")
}

table1_example <- function(keyword, example_value) {
  px_function <- keyword_to_function(keyword)

  str <-
    stringr::str_glue(
      "@examples",
      "# Set {keyword}",
      "x1 <-",
      "   px(population_gl) |>",
      "   {px_function}('{example_value}')",
      "",
      "# Print {keyword}",
      "{px_function}(x1)",
      .sep = "\n"
    )

  if (!keyword %in% mandatory_keywords()) {
    str <-
      stringr::str_glue(
        "{str}",
        "",
        "# Remove {keyword}",
        "x2 <- {px_function}(x1, NULL)",
        "{px_function}(x2)",
        .sep = "\n"
      )
  }

  return(str)
}

add_table1_example <- add_documentation_function(table1_example)

table2_example <- function(keyword, example_value1, example_value2) {
  px_function <- keyword_to_function(keyword)

  str <-
    stringr::str_glue(
    "@examples",
    "# Set {keyword} for all languages",
    "x1 <-",
    "  px(population_gl) |>",
    "  {px_function}('{example_value1}')",
    "",
    "# Print {keyword}",
    "{px_function}(x1)",
    "",
    "# Set {keyword} for individual languages",
    "library(tibble)",
    "x2 <-",
    "  x1 |>",
    "  px_languages(c('en', 'kl')) |>",
    "  {px_function}(tribble(~language, ~value,",
    "                      'en', '{example_value1}',",
    "                      'kl', '{example_value2}'))",
    "{px_function}(x2)",
    .sep = "\n"
    )

  if (!keyword %in% c(mandatory_keywords(), "TITLE")) {
    str <-
      stringr::str_glue(
        "{str}",
        "",
        "# Remove {keyword}",
        "x3 <- {px_function}(x2, NULL)",
        "{px_function}(x3)",
        .sep = "\n"
        )
  }

  return(str)
}

variables2_example <- function(keyword, example_value1, example_value2, example_value3) {
  if (is.na(example_value1)) {
    return("")
  }

  px_function <- keyword_to_function(keyword)

  stringr::str_glue(
    "@examples",
    "# Set {keyword} for all languages",
    "x1 <-",
    "  px(population_gl) |>",
    "  {px_function}('{example_value1}')",
    "",
    "# Print {keyword}",
    "{px_function}(x1)",
    "",
    "# Set {keyword} for individual variables",
    "library(tibble)",
    "x2 <-",
    "  x1 |>",
    "  {px_function}(tribble(~`variable-code`, ~{tolower(keyword)},",
    "                    'gender', '{example_value2}',",
    "                    'age',    '{example_value3}'))",
    "{px_function}(x2)",
    "",
    "# Set {keyword} for individual languages",
    "x3 <-",
    "  x2 %>%",
    "  px_languages(c('en', 'kl')) |>",
    "  {px_function}(tribble(~`variable-code`, ~language, ~{tolower(keyword)},",
    "                    'gender',    'en',      '{example_value2}_en',",
    "                    'gender',    'kl',      '{example_value2}_kl',",
    "                    'age',       'en',      '{example_value3}_en'))",
    "{px_function}(x3)",
    "",
    "# Remove {keyword}",
    "x4 <- {px_function}(x3, NULL)",
    "{px_function}(x4)",
    .sep = "\n"
  )
}

cells1_example <- function(keyword, example_value1, example_value2) {
  px_function <- keyword_to_function(keyword)

  stringr::str_glue(
    "@examples",
    "# Set {keyword} for a variable",
    "library(tibble)",
    "x1 <-",
    "  population_gl |>",
    "  px() |>",
    "  {px_function}(tribble(~`variable-code`, ~{tolower(keyword)},",
    "                       'gender', {example_value1}))",
    "",
    "# Print {keyword}",
    "{px_function}(x1)",
    "",
    "# Set {keyword} for a value",
    "x2 <-",
    "  x1 |>",
    "  {px_function}(tribble(~`variable-code`, ~code, ~{tolower(keyword)},",
    "                       'age', '2004', {example_value2}))",
    "{px_function}(x2)",
    "",
    "# Remove {keyword}",
    "x3 <- {px_function}(x2, NULL)",
    "{px_function}(x3)",
    .sep = "\n"
  )
}

add_cells1_example <- add_documentation_function(cells1_example)

cells2_example <- function(keyword, example_value1, example_value2, example_value3) {
  px_function <- keyword_to_function(keyword)

  stringr::str_glue(
    "@examples",
    "# Set {keyword} for a value",
    "library(tibble)",
    "x1 <-",
    "  population_gl |>",
    "  px() |>",
    "  {px_function}(",
    "    tribble(~`variable-code`, ~code,  ~{tolower(keyword)},",
    "            'year', '2004', '{example_value1}'))",
    "",
    "# Print {keyword}",
    "{px_function}(x1)",
    "",
    "# Set {keyword} for a value in specific language",
    "x2 <-",
    "  x1 |>",
    "  px_languages(c('en', 'kl')) |>",
    "  {px_function}(",
    "    tribble(~`variable-code`, ~code,  ~language, ~{tolower(keyword)},",
    "            'age', '0-6', 'en', '{example_value2}',",
    "            'age', '0-6', 'kl', '{example_value3}'))",
    "{px_function}(x2)",
    "",
    "# Remove {keyword}",
    "x3 <- {px_function}(x2, NULL)",
    "{px_function}(x3)",
    .sep = "\n"
  )
}

add_cells2_example <- add_documentation_function(cells2_example)

acrosscells_example <- function(keyword) {
  px_function <- keyword_to_function(keyword)

  stringr::str_glue(
    "@examples",
    "# Set {keyword} for a value",
    "library(tibble)",
    "x1 <-",
    "  population_gl |>",
    "  px() |>",
    "  {px_function}(",
    "    tribble(~gender,  ~age,  ~year, ~cellnote,",
    "             'male', '0-6', '2004', 'Approximation'))",
    "",
    "x2 <-",
    "  x1 |>",
    "  px_cellnote(",
    "    tribble(~gender,   ~age,  ~year, ~cellnote,",
    "           'female',    '*', '2014', 'Uncertainty in ages'))",
    "",
    "# Print {keyword}",
    "{px_function}(x2)",
    "",
    "# Set {keyword} in multiple languagese",
    "x3 <-",
    "  x1 |>",
    "  px_languages(c('en', 'kl')) |>",
    "  {px_function}(",
    "    tribble(~age, ~year, ~language, ~cellnote,",
    "             '*', '2003',  'en', 'Some of the figures are from 2003',",
    "             '*', '2003', 'kl', 'Kisitsisit ilaat 2003-imeersuupput'))",
    "{px_function}(x3)",
    "",
    "# Remove {keyword}",
    "x4 <- {px_function}(x3, NULL)",
    "{px_function}(x4)",
    .sep = "\n"
  )
}

add_acrosscells_example <- add_documentation_function(acrosscells_example)

param_validate <- function() {
  stringr::str_glue(
    "@param validate Optional. If TRUE a number of validation checks are performed ",
    "on the px object, and an error is thrown if the object is not valid. If FALSE, ",
    "the checks are skipped, which can be usefull for large px objects where the ",
    "check can be time consuming. Use [px_validate()] to manually preform the check."
  )
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
      \\item Use character to set {keyword} for all languages and STUB/HEADING
      variables.
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
    "with the names of the STUB and HEADING variables.",
    " The '{colname}' column is ",
    "the {colname} text, and the STUB/HEADING columns control which cells the ",
    "note applies to. Use asterisk (*) if a note applies to all cells in a ",
    "variable. Use column 'language'  to set {keyword} for specific ",
    "languages. If 'value' is missing, the current {keyword} is returned. If ",
    "value is NULL, {keyword} is removed."
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

pivot_param_value <- function(keyword) {
  stringr::str_glue(
    "
    Optional. A character vector of variable names to change to
    STUB. This also changes the {keyword} order. With names in `variables`
    becoming 1, 2, ... If missing, the current {keyword} variables are returned.
    "
  )
}

return_px_or_df <- function(keyword=NULL) {
  "@return A px object or data frame."
}

add_return_px_or_df <- add_documentation_function(return_px_or_df)

return_px_or_char_str <- function(keyword=NULL) {
  "@return A px object or a character string."
}

add_return_px_or_char_str <- add_documentation_function(return_px_or_char_str)

return_px_or_char_vector <- function() {
  "@return A px object or a character vector."
}
return_px_or_char_vector_or_df <- function() {
  "@return A px object, a character string, or a data frame."
}


