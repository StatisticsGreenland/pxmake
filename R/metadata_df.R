#' Regular expression to parse header in PX-file
#'
#' @returns A character vector
#' @keywords internal
get_px_metadata_regex <- function() {
  paste0("(?<keyword>[[:upper:]-]+)",    # Leading keyword
         "(?:\\[)?",                     # Maybe opening language bracket [
         "(?<language>[[:alpha:]_-]+)?", # Maybe language
         "(?:\\])?",                     # Maybe closing language bracket ]
         "(?:\\(\")?",                   # Maybe opening sub-key parentheses (
         "(?<variable>[^\"=]+)?",        # Maybe sub-key
         "(?:\",\")?",                   # Maybe comma before cell value
         "(?<cell>[^=]+?)?(?=\\)=|=)",   # Maybe cell value
         "(?:\")?",                      # Maybe closing " after cell value
         "(?:\"\\))?",                   # Maybe closing sub-key parentheses )
         "=",                            # definitely =
         "(?<value>[^;]*)",              # Value is everything up to ending ;
         "(?:;$)?"                       # Maybe ;
  )
}

#' Get metadata df from px lines
#'
#' @param metadata_lines A character vector with the header of a PX-file.
#'
#' @returns A data frame
#' @keywords internal
get_metadata_df_from_px_lines <- function(metadata_lines) {
  acrosscells_keywords <- c("CELLNOTE", "CELLNOTEX")

  metadata_lines %>%
    # Remove newlines in file. Use semi-colon as line separator
    paste0(collapse = "") %>%
    stringr::str_split(";") %>%
    unlist() %>%
    stringr::str_match(get_px_metadata_regex()) %>%
    dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(...,
                                                          repair = "unique",
                                                          quiet = TRUE
                                                          )
                     ) %>%
    dplyr::select(-starts_with("...")) %>%
    dplyr::mutate(cell = stringr::str_replace_all(.data$cell, '"\\)', ''),
                  cell = dplyr::na_if(.data$cell, '')
                  ) %>%
    # remove leading and trailing " on all keywords except TIMEVAL
    dplyr::mutate(value = ifelse(.data$keyword != "TIMEVAL",
                                 stringr::str_replace_all(.data$value, '^"|"$', ''),
                                 .data$value
                                 )
                  ) %>%
    # Variables indexed by CONTVARIABLE are cell values not variable
    dplyr::mutate(cell = ifelse(.data$keyword %in% keywords_indexed_by_contvariable(),
                                .data$variable,
                                .data$cell
                                ),
                  variable = ifelse(.data$keyword %in% keywords_indexed_by_contvariable(),
                                    NA,
                                    .data$variable
                                    )
                  ) %>%
    dplyr::mutate(variable = dplyr::if_else(.data$keyword %in% acrosscells_keywords,
                                            stringr::str_glue('"{variable}","{cell}"'),
                                            .data$variable
                                            ),
                  cell = ifelse(.data$keyword %in% acrosscells_keywords,
                                NA,
                                .data$cell
                                )
                  ) %>%
    # remove double quotes caused by collapsing values spanning multiple lines
    dplyr::mutate(value = stringr::str_replace_all(.data$value, '""', '')) %>%
    dplyr::mutate(value = ifelse(.data$keyword != "TIMEVAL",
                                 stringr::str_split(.data$value, '","'),
                                 .data$value
                                 )
                  ) %>%
    dplyr::filter(.data$keyword != "DATA")
}

#' Get variable code and label
#'
#' Get a data frame with variable codes and label in all languages.
#' If VARIABLECODE is used, the relation is defined there, but otherwise the
#' variable code is set to be the main languages' label.
#'
#' @inheritParams sort_metadata_df
#'
#' @returns A data frame
#' @keywords internal
get_variable_label <- function(metadata_df) {
  metadata_df <- add_main_language(metadata_df)

  head_stub <-
    metadata_df %>%
    dplyr::filter(.data$keyword %in% c("HEADING", "STUB")) %>%
    tidyr::unnest("value") %>%
    dplyr::rename("variable-label" = "value") %>%
    dplyr::group_by(.data$keyword, .data$language) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select("keyword", "language", "main_language", "variable-label", "index")

  head_stub_main_language <-
    head_stub %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::rename("main_language_label" = "variable-label") %>%
    dplyr::select("keyword", "index", "main_language_label")

  metadata_df %>%
    dplyr::filter(.data$keyword == "VARIABLECODE") %>%
    tidyr::unnest("value") %>%
    dplyr::select("variable-code" = "value",
                  "variable-label" = "variable",
                  "language",
                  "main_language"
                  ) %>%
    dplyr::full_join(head_stub,
                     by = c("variable-label", "language", "main_language")
                     ) %>%
    dplyr::left_join(head_stub_main_language, by = c("keyword", "index")) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(.data$`variable-code`),
                                           .data$main_language_label,
                                           .data$`variable-code`
                                           )
                  ) %>%
    dplyr::select(-"main_language_label")
}

#' Add boolean main_language column
#'
#' @inheritParams sort_metadata_df
#'
#' @returns A data frame
#' @keywords internal
add_main_language <- function(metadata_df) {
  m_language <- get_main_language(metadata_df)

  if (is.na(m_language)) {
    metadata_df %>%
      dplyr::mutate(main_language = is.na(m_language))
  } else {
    metadata_df %>%
      replace_na_language_with_main_language() %>%
      dplyr::mutate(main_language = .data$language == m_language)
  }
}

#' Get the main language from metadata
#'
#' @inheritParams sort_metadata_df
#'
#' @returns Character
#' @keywords internal
get_main_language <- function(metadata_df) {
  main_language <-
    metadata_df %>%
    dplyr::filter(.data$keyword %in% c("LANGUAGE", "LANGUAGES")) %>%
    dplyr::arrange(.data$keyword) %>%
    tidyr::unnest("value") %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$value)

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
#' @returns A data frame
#' @keywords internal
replace_na_language_with_main_language <- function(metadata_df) {
  metadata_df %>%
    dplyr::mutate(language = tidyr::replace_na(.data$language,
                                               get_main_language(.)
                                               )
                  )
}

#' Sort metadata data frame
#'
#' The data frame is first sorted by the keyword order defined in the
#' px-specification and then by the language order.
#'
#' @param metadata_df Data frame with metadata.
#'
#' @returns A data frame
#' @keywords internal
sort_metadata_df <- function(metadata_df) {
  languages <-
    metadata_df %>%
    dplyr::filter(.data$keyword == "LANGUAGES") %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(language_order = dplyr::row_number()) %>%
    dplyr::select("language" = "value", "language_order")

  stub_heading <-
    metadata_df %>%
    dplyr::filter(.data$keyword %in% c("STUB", "HEADING"),
                  .data$language %in% get_main_language(.)
                  ) %>%
    dplyr::arrange(desc(.data$keyword)) %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(stub_heading_order = dplyr::row_number()) %>%
    dplyr::select("variable" = "value", "stub_heading_order")

  metadata_df %>%
    dplyr::left_join(px_keywords %>% dplyr::select('keyword', 'order'),
                     by = "keyword"
                     ) %>%
    dplyr::left_join(languages, by = "language") %>%
    dplyr::left_join(stub_heading, by = "variable") %>%
    dplyr::arrange(.data$order,
                   .data$keyword,
                   .data$stub_heading_order,
                   .data$language_order,
                   !is.na(.data$variable),
                   .data$variable,
                   .data$cell
                   ) %>%
    dplyr::select(-"order", -"language_order", -"stub_heading_order")
}

#' Get metadata df from px object
#'
#' @param x A px object
#'
#' @returns A data frame
#' @keywords internal
get_metadata_df_from_px <- function(x) {
  metadata_template <- dplyr::tibble(keyword  = character(),
                                     language = character(),
                                     variable = character(),
                                     cell     = character(),
                                     value    = list(character())
                                     )

  languages <-
    x$languages %>%
    dplyr::summarise(value = list(paste0(.data$language, sep = ""))) %>%
    dplyr::mutate(keyword = "LANGUAGES") %>%
    dplyr::filter(.data$value != "")

  table <-
    x$table1 %>%
    tidyr::drop_na("value") %>%
    wrap_varaible_in_list("value")

  table_language_dependent <-
    x$table2 %>%
    dplyr::rename("variable" = "code") %>%
    tidyr::drop_na("value") %>%
    wrap_varaible_in_list("value")

  variablecode <-
    x$variables2 %>%
    dplyr::distinct("keyword" = "VARIABLECODE",
                    .data$language,
                    "variable" = .data$`variable-label`,
                    "value" = .data$`variable-code`
                    ) %>%
    tidyr::drop_na("value") %>%
    wrap_varaible_in_list("value")

  note_etc <-
    x$variables2 %>%
    tidyr::pivot_longer(cols = c(-"variable-code", -"language", -"variable-label"),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(.data$keyword)) %>%
    dplyr::select("keyword", "variable" = "variable-label", "language", "value") %>%
    dplyr::arrange_all() %>%
    tidyr::drop_na("value") %>%
    wrap_varaible_in_list("value")

  name_relation <-
    dplyr::select(x$variables2, "variable-code", "language", "variable-label")

  variables1 <-
    x$variables1 %>%
    dplyr::left_join(name_relation, by = "variable-code")

  stub_heading_variables <-
    variables1 %>%
    dplyr::filter(toupper(.data$pivot) %in% c("STUB", "HEADING")) %>%
    dplyr::distinct(.data$`variable-code`) %>%
    dplyr::pull("variable-code")

  head_stub <-
    variables1 %>%
    dplyr::filter(.data$`variable-code` %in% stub_heading_variables) %>%
    dplyr::mutate(keyword = toupper(.data$pivot)) %>%
    dplyr::arrange(.data$keyword, .data$order) %>%
    dplyr::group_by(.data$keyword, .data$language) %>%
    dplyr::summarise(value = list(paste(.data$`variable-label`, sep = ", ")),
                     .groups = "keep"
                     )

  variable_type <-
    variables1 %>%
    tidyr::drop_na("variable-type") %>%
    dplyr::filter(! toupper(.data$`variable-type`) %in% c("CONTVARIABLE")) %>%
    tidyr::pivot_longer(cols= "variable-type",
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(.data$keyword)) %>%
    dplyr::select("keyword", "variable" = "variable-label", "language", "value") %>%
    wrap_varaible_in_list("value")

  time_metadata <-
    variables1 %>%
    dplyr::filter(.data$timeval)

  time_variable <-
    time_metadata %>%
    dplyr::distinct(.data$`variable-code`) %>%
    dplyr::pull("variable-code")

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
      dplyr::select("keyword", "language", "variable" = "variable-label", "value") %>%
      wrap_varaible_in_list("value")
  }

  codes_not_in_cells <-
    x$data %>%
    dplyr::select(dplyr::all_of(intersect(stub_heading_variables, names(.)))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable-code",
                        values_to = "code"
                        ) %>%
    dplyr::distinct_all() %>%
    align_data_frames(get_base_cells2()) %>%
    dplyr::select("variable-code", "code") %>%
    dplyr::anti_join(x$cells2, by = c("variable-code", "code")) %>%
    dplyr::mutate(value = .data$code) %>%
    tidyr::crossing(language = defined_languages(x))

  cells <-
    x$cells2 %>%
    dplyr::bind_rows(codes_not_in_cells) %>%
    dplyr::left_join(x$cells1, by = c("variable-code", "code")) %>%
    dplyr::left_join(name_relation, by = c("variable-code", "language")) %>%
    dplyr::mutate(value = ifelse(is.na(.data$value), .data$code, .data$value))

  code_value <-
    cells %>%
    tidyr::pivot_longer(cols = c("code", "value"), names_to = "type") %>%
    dplyr::mutate(keyword = toupper(paste0(.data$type, "s"))) %>%
    dplyr::arrange(.data$keyword, .data$order) %>%
    dplyr::rename("variable" = "variable-label") %>%
    dplyr::group_by(.data$keyword, .data$language, .data$variable) %>%
    dplyr::summarise(value = list(paste(.data$value, sep = ", ")), .groups = "keep") %>%
    dplyr::ungroup()

  valuenotes <-
    cells %>%
    tidyr::pivot_longer(cols = starts_with("valuenote"),
                        names_to = "keyword",
                        values_to = "keyword_value"
                        ) %>%
    dplyr::mutate(keyword = toupper(.data$keyword)) %>%
    tidyr::drop_na("keyword_value") %>%
    wrap_varaible_in_list("keyword_value") %>%
    dplyr::select("keyword",
                  "language",
                  "variable" = "variable-label",
                  "cell" = "value",
                  "value" = "keyword_value"
                  )

  precision <-
    cells %>%
    dplyr::mutate(keyword = "PRECISION") %>%
    tidyr::drop_na("precision") %>%
    wrap_varaible_in_list("precision") %>%
    dplyr::select("keyword",
                  "language",
                  "variable" = "variable-label",
                  "cell" = "value",
                  "value" = "precision"
                  )

  acrosscells <-
    x$acrosscells %>%
    dplyr::mutate(across(c(px_stub(x), px_heading(x)), ~ifelse(is.na(.), "*", .))) %>%
    tidyr::pivot_longer(cols = setdiff(get_base_acrosscells() %>% names(), "language"),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    dplyr::mutate(keyword = toupper(.data$keyword)) %>%
    tidyr::drop_na("value") %>%
    tidyr::unite("variable", all_of(stub_heading_variables), sep = '","') %>%
    wrap_varaible_in_list("value")

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
                     acrosscells
                     ) %>%
    replace_na_language_with_main_language() %>%
    sort_metadata_df()

  return(metadata_df)
}
