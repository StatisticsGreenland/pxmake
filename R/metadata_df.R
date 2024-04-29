#' Regular expression to parse header in px file
#'
#' @return A character vector
get_px_metadata_regex <- function() {
  paste0("(?<keyword>[[:upper:]-]+)",    # Leading keyword
         "(?:\\[)?",                     # Maybe opening language bracket [
         "(?<language>[[:alpha:]_-]+)?", # Maybe language
         "(?:\\])?",                     # Maybe closing language bracket ]
         "(?:\\(\")?",                   # Maybe opening sub-key parentheses (
         "(?<variable>[^\"]+)?",         # Maybe sub-key
         "(?:\",\")?",                   # Maybe comma before cell value
         "(?<cell>[^\"]+)?",             # Maybe cell value
         "(?:\")?",                      # Maybe closing " after cell value
         "(?:\"\\))?",                   # Maybe closing sub-key parentheses )
         "=",                            # definitely =
         "(?<value>[^;]*)",              # Value is everything up to ending ;
         "(?:;$)?"                       # Maybe ;
  )
}

#' Get metadata df from px lines
#'
#' @param metadata_lines A character vector with the header of a px file.
#'
#' @return A data frame
get_metadata_df_from_px_lines <- function(metadata_lines) {
  keywords_indexed_by_contvariable <-
    get_px_keywords() %>%
    dplyr::filter(indexed_by_contvariable) %>%
    dplyr::pull(keyword)

  acrosscell_keywords <- c("CELLNOTE", "CELLNOTEX")

  metadata_lines %>%
    # Remove newlines in file. Use semi-colon as line separator
    paste0(collapse = "") %>%
    stringr::str_split(";") %>%
    unlist() %>%
    stringr::str_match(get_px_metadata_regex()) %>%
    magrittr::extract(,-1) %>% # remove full match column
    dplyr::as_tibble() %>%
    # remove leading and trailing " on all keywords except TIMEVAL
    dplyr::mutate(value = ifelse(keyword != "TIMEVAL",
                                 stringr::str_replace_all(value, '^"|"$', ''),
                                 value
                                 )
                  ) %>%
    # Variables indexed by CONTVARIABLE are cell values not variable
    dplyr::mutate(cell = ifelse(keyword %in% keywords_indexed_by_contvariable,
                                variable,
                                cell
                                ),
                  variable = ifelse(keyword %in% keywords_indexed_by_contvariable,
                                    NA,
                                    variable
                                    )
                  ) %>%
    dplyr::mutate(variable = dplyr::if_else(keyword %in% acrosscell_keywords,
                                            stringr::str_glue('"{variable}","{cell}"'),
                                            variable
                                            ),
                  cell = ifelse(keyword %in% acrosscell_keywords,
                                NA,
                                cell
                                )
                  ) %>%
    # remove double quotes caused by collapsing values spanning multiple lines
    dplyr::mutate(value = stringr::str_replace_all(value, '""', '')) %>%
    dplyr::mutate(value = ifelse(keyword != "TIMEVAL",
                                 stringr::str_split(value, '","'),
                                 value
                                 )
                  ) %>%
    dplyr::filter(keyword != "DATA")
}

#' Get encoding name from metadata
#'
#' @inherit get_main_language
get_encoding_from_metadata <- function(metadata_df) {
  encoding_str <-
    metadata_df %>%
    dplyr::filter(keyword == "CODEPAGE") %>%
    tidyr::unnest(value) %>%
    dplyr::pull(value)

  if (length(encoding_str) == 0) {
    encoding_str <- get_default_encoding()
  }

  return(encoding_str)
}

#' Get variable code and label
#'
#' Get a data frame with variable codes and label in all languages.
#' If VARIABLECODE is used, the relation is defined there, but otherwise the
#' variable code is set to be the main languages' label.
#'
#' @inheritParams sort_metadata_df
#'
#' @return A data frame
get_variable_label <- function(metadata_df) {
  metadata_df <- add_main_language(metadata_df)

  head_stub <-
    metadata_df %>%
    dplyr::filter(keyword %in% c("HEADING", "STUB")) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(`variable-label` = value) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(keyword, language, main_language, `variable-label`, index)

  head_stub_main_language <-
    dplyr::filter(head_stub, main_language) %>%
    dplyr::rename(main_language_label = `variable-label`) %>%
    dplyr::select(keyword, index, main_language_label)

  metadata_df %>%
    dplyr::filter(keyword == "VARIABLECODE") %>%
    tidyr::unnest(value) %>%
    dplyr::select(`variable-code` = value,
                  `variable-label` = variable,
                  language,
                  main_language
                  ) %>%
    dplyr::full_join(head_stub,
                     by = c("variable-label", "language", "main_language")
                     ) %>%
    dplyr::left_join(head_stub_main_language, by = c("keyword", "index")) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(`variable-code`),
                                           main_language_label,
                                           `variable-code`
                                           )
                  ) %>%
    dplyr::select(-main_language_label)
}

#' Add boolean main_language column
#'
#' @inheritParams sort_metadata_df
#'
#' @return A data frame
add_main_language <- function(metadata_df) {
  m_language <- get_main_language(metadata_df)

  if (is.na(m_language)) {
    metadata_df %>%
      dplyr::mutate(main_language = is.na(m_language))
  } else {
    metadata_df %>%
      replace_na_language_with_main_language() %>%
      dplyr::mutate(main_language = language == m_language)
  }
}

#' Get the main language from metadata
#'
#' @inheritParams sort_metadata_df
#'
#' @return Character
get_main_language <- function(metadata_df) {
  main_language <-
    metadata_df %>%
    dplyr::filter(keyword %in% c("LANGUAGE", "LANGUAGES")) %>%
    dplyr::arrange(keyword) %>%
    tidyr::unnest(value) %>%
    dplyr::slice(1) %>%
    dplyr::pull(value)

  if (length(main_language) == 0) {
    main_language <- NA
  }

  return(main_language)
}

#' Impute missing language
#'
#' Add main language as language if language is missing.
#'
#' @inheritParams sort_metadata_df
#'
#' @return A data frame
replace_na_language_with_main_language <- function(metadata_df) {
  metadata_df %>%
    dplyr::mutate(language = tidyr::replace_na(language, get_main_language(.)))
}

#' Sort metadata data frame
#'
#' The data frame is first sorted by the keyword order defined in the
#' px-specification and then by the language order.
#'
#' @param metadata_df Data frame with metadata.
#'
#' @return A data frame
sort_metadata_df <- function(metadata_df) {
  languages <-
    metadata_df %>%
    dplyr::filter(keyword == "LANGUAGES") %>%
    tidyr::unnest(value) %>%
    dplyr::mutate(language_order = dplyr::row_number()) %>%
    dplyr::select(language = value, language_order)

  metadata_df %>%
    dplyr::left_join(get_px_keywords() %>% dplyr::select('keyword', 'order'),
                     by = "keyword"
    ) %>%
    dplyr::left_join(languages, by = "language") %>%
    dplyr::arrange(order, keyword, language_order,
                   !is.na(variable), variable, cell
    ) %>%
    dplyr::select(-order, -language_order)
}

#' Get metadata df from px object
#'
#' @param x A px object
#'
#' @return A data frame
get_metadata_df_from_px <- function(x) {
  metadata_template <- dplyr::tibble(keyword  = character(),
                                     language = character(),
                                     variable = character(),
                                     cell     = character(),
                                     value    = list(character())
                                     )

  languages <-
    x$languages %>%
    dplyr::summarise(value = list(paste0(language, sep = ""))) %>%
    dplyr::mutate(keyword = "LANGUAGES") %>%
    dplyr::filter(value != "")

  table <-
    x$table1 %>%
    tidyr::drop_na(value) %>%
    wrap_varaible_in_list(value)

  table_language_dependent <-
    x$table2 %>%
    dplyr::rename(variable = code) %>%
    tidyr::drop_na(value) %>%
    wrap_varaible_in_list(value)

  variablecode <-
    x$variables2 %>%
    dplyr::distinct(keyword = "VARIABLECODE",
                    language,
                    variable = `variable-label`,
                    value = `variable-code`
                    ) %>%
    tidyr::drop_na(value) %>%
    wrap_varaible_in_list(value)

  note_etc <-
    x$variables2 %>%
    tidyr::pivot_longer(cols = c(-`variable-code`, -language, -`variable-label`),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    dplyr::select(keyword, variable = `variable-label`, language, value) %>%
    dplyr::arrange_all() %>%
    tidyr::drop_na(value) %>%
    wrap_varaible_in_list(value)

  name_relation <-
    dplyr::select(x$variables2, `variable-code`, language, `variable-label`)

  variables1 <-
    x$variables1 %>%
    dplyr::left_join(name_relation, by = "variable-code")

  stub_heading_variables <-
    variables1 %>%
    dplyr::filter(toupper(pivot) %in% c("STUB", "HEADING")) %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  head_stub <-
    variables1 %>%
    dplyr::filter(`variable-code` %in% stub_heading_variables) %>%
    dplyr::mutate(keyword = toupper(pivot)) %>%
    dplyr::arrange(keyword, order) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::summarise(value = list(paste(`variable-label`, sep = ", ")),
                     .groups = "keep"
                     )

  variable_type <-
    variables1 %>%
    tidyr::drop_na(type) %>%
    dplyr::filter(! toupper(type) %in% c("TIME", "CONTVARIABLE")) %>%
    dplyr::rename(`variable-type` = type) %>%
    tidyr::pivot_longer(`variable-type`,
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    dplyr::select(keyword, variable = `variable-label`, language, value) %>%
    wrap_varaible_in_list(value)

  time_metadata <-
    variables1 %>%
    dplyr::filter(toupper(type) == "TIME")

  time_variable <-
    time_metadata %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  if (length(time_variable) == 0) {
    timeval <- NULL
  } else {
    error_if_more_than_one_time_variable(time_variable)

    time_values <-
      x$data %>%
      dplyr::distinct(across(all_of(time_variable))) %>%
      dplyr::pull(1)

    timeval <-
      time_metadata %>%
      dplyr::mutate(keyword = "TIMEVAL",
                    value = format_time_values(time_values)
      ) %>%
      dplyr::select(keyword, language, variable = `variable-label`, value) %>%
      wrap_varaible_in_list(value)
  }

  codes_not_in_codelist <-
    x$data %>%
    dplyr::select(dplyr::all_of(intersect(stub_heading_variables, names(.)))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable-code",
                        values_to = "code"
                        ) %>%
    dplyr::distinct_all() %>%
    align_data_frames(get_base_codelists2()) %>%
    dplyr::select(`variable-code`, code) %>%
    dplyr::anti_join(x$codelists2, by = c("variable-code", "code")) %>%
    dplyr::mutate(value = code) %>%
    tidyr::crossing(language = defined_languages(x))

  codelists <-
    x$codelists2 %>%
    dplyr::bind_rows(codes_not_in_codelist) %>%
    dplyr::left_join(x$codelists1, by = c("variable-code", "code")) %>%
    dplyr::left_join(name_relation, by = c("variable-code", "language")) %>%
    dplyr::mutate(value = ifelse(is.na(value), code, value))

  code_value <-
    codelists %>%
    tidyr::pivot_longer(cols = c("code", "value"), names_to = "type") %>%
    dplyr::mutate(keyword = toupper(paste0(type, "s"))) %>%
    dplyr::arrange(keyword, order) %>%
    dplyr::rename(variable = `variable-label`) %>%
    dplyr::group_by(keyword, language, variable) %>%
    dplyr::summarise(value = list(paste(value, sep = ", ")), .groups = "keep") %>%
    dplyr::ungroup()

  valuenotes <-
    codelists %>%
    tidyr::pivot_longer(cols = starts_with("valuenote"),
                        names_to = "keyword",
                        values_to = "keyword_value"
                        ) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    tidyr::drop_na(keyword_value) %>%
    wrap_varaible_in_list(keyword_value) %>%
    dplyr::select(keyword,
                  language,
                  variable = `variable-label`,
                  cell = value,
                  value = keyword_value
                  )

  precision <-
    codelists %>%
    dplyr::mutate(keyword = "PRECISION") %>%
    tidyr::drop_na(precision) %>%
    wrap_varaible_in_list(precision) %>%
    dplyr::select(keyword,
                  language,
                  variable = `variable-label`,
                  cell = value,
                  value = precision
                  )

  acrosscell <-
    x$acrosscell %>%
    dplyr::mutate(across(c(stub(x), heading(x)), ~ifelse(is.na(.), "*", .))) %>%
    tidyr::pivot_longer(cols = setdiff(get_base_acrosscell() %>% names(), "language"),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    tidyr::drop_na(value) %>%
    tidyr::unite("variable", all_of(stub_heading_variables), sep = '","') %>%
    #dplyr::mutate(variable = str_quote(variable)) %>%
    wrap_varaible_in_list(value)

  metadata_df <-
    dplyr::bind_rows(metadata_template,
                     languages,
                     table,
                     table_language_dependent,
                     variablecode,
                     note_etc,
                     head_stub,
                     variable_type,
                     timeval,
                     code_value,
                     valuenotes,
                     precision,
                     acrosscell
                     ) %>%
    replace_na_language_with_main_language() %>%
    sort_metadata_df()

  return(metadata_df)
}
