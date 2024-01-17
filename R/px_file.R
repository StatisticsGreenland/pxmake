#' Regular expression to parse header in px file
#'
#' @returns A character vector
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
#' @returns A data frame
get_metadata_df_from_px_lines <- function(metadata_lines) {
  keywords_indexed_by_contvariable <-
    get_px_keywords() %>%
    dplyr::filter(indexed_by_contvariable) %>%
    dplyr::pull(keyword)

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
    # remove double quotes caused by collapsing values spanning multiple lines
    dplyr::mutate(value = stringr::str_replace_all(value, '""', '')) %>%
    dplyr::mutate(value = ifelse(keyword != "TIMEVAL",
                                 stringr::str_split(value, '","'),
                                 value
    )
    ) %>%
    dplyr::filter(keyword != "DATA")
}

#' Format df for px format
#'
#' Turn all variables, except figures variable, into character and replace NA
#' with dash.
#'
#' @param data_df A data frame with data.
#' @param figures_variable Character. The name of the figures variable.
#'
#' @returns A data frame
format_data_df <- function(data_df, figures_variable) {
  data_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(-one_of(intersect(names(.), figures_variable)),
                         as.character
    )
    ) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ tidyr::replace_na(.x, "-")
    )
    )
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

#' Sort metadata data frame
#'
#' The data frame is first sorted by the keyword order defined in the
#' px-specification and then by the language order.
#'
#' @param metadata_df Data frame with metadata.
#'
#' @returns A data frame
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

#' Get data cube used in px file format
#'
#' @inheritParams sort_metadata_df
#' @inheritParams format_data_df
#'
#' @returns A data frame
get_data_cube <- function(metadata_df, data_df) {
  metadata_df <- add_main_language(metadata_df)

  metadata_df_main_language <-
    metadata_df %>%
    dplyr::filter(main_language)

  labels <-
    metadata_df_main_language %>%
    dplyr::filter(keyword == "VARIABLECODE") %>%
    tidyr::unnest(value) %>%
    dplyr::select(label = variable, variable = value)

  stub_and_heading_df <-
    metadata_df_main_language %>%
    dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
    tidyr::unnest(value) %>%
    dplyr::select(keyword, label = value) %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::mutate(variable = ifelse(is.na(variable),
                                    label,
                                    variable
    )
    )

  stub_vars <-
    stub_and_heading_df %>%
    dplyr::filter(keyword == "STUB") %>%
    dplyr::pull(variable) %>%
    intersect(names(data_df))

  heading_vars <-
    stub_and_heading_df %>%
    dplyr::filter(keyword == "HEADING") %>%
    dplyr::pull(variable) %>%
    intersect(names(data_df))

  head_stub_variable_names <- c(heading_vars, stub_vars)

  figures_var <- setdiff(names(data_df),  head_stub_variable_names)

  error_if_not_exactly_one_figures_variable(figures_var)

  codelist <-
    metadata_df %>%
    dplyr::filter(keyword == "CODES", main_language) %>%
    dplyr::select(keyword, variable, value) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(label = variable) %>%
    dplyr::group_by(label) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::filter(variable %in% names(data_df)) %>%
    dplyr::select(variable, sortorder, code = value)

  # Complete data by adding all combinations of variable values in data and
  # codelist
  data_values <-
    data_df %>%
    dplyr::select(dplyr::all_of(head_stub_variable_names)) %>%
    lst_distinct_and_arrange()

  codelist_values <-
    split(codelist$code, codelist$variable) %>%
    lst_distinct_and_arrange()

  data_and_codelist_values <- merge_named_lists(data_values, codelist_values)

  heading_code_vars      <- paste0("code_",      heading_vars, recycle0 = TRUE)
  heading_sortorder_vars <- paste0("sortorder_", heading_vars, recycle0 = TRUE)
  stub_code_vars         <- paste0("code_",      stub_vars,    recycle0 = TRUE)
  stub_sortorder_vars    <- paste0("sortorder_", stub_vars,    recycle0 = TRUE)

  data_cube <-
    data_df %>%
    tidyr::complete(!!!data_and_codelist_values) %>%
    dplyr::mutate(id_ = dplyr::row_number()) %>% # used to unpivot data later
    tidyr::pivot_longer(cols = all_of(head_stub_variable_names),
                        names_to = "variable",
                        values_to = "code"
    ) %>%
    dplyr::left_join(codelist, by = c("variable", "code")) %>%
    tidyr::pivot_wider(names_from = variable,
                       values_from = c("code", "sortorder")
    ) %>%
    dplyr::select(-id_) %>%
    # Sort by sortorder for first heading var, codes for first heading var,
    # sortorder for second heading var, etc.
    dplyr::arrange(dplyr::across(zip_vectors(heading_sortorder_vars, heading_code_vars))) %>%
    dplyr::select(-all_of(heading_sortorder_vars)) %>%
    { if(length(heading_code_vars > 0)) {
      tidyr::pivot_wider(.,
                         names_from = all_of(heading_code_vars),
                         values_from = all_of(figures_var)
      )
    } else {
      .
    }
    } %>%
    dplyr::arrange(dplyr::across(zip_vectors(stub_sortorder_vars, stub_code_vars))) %>%
    dplyr::select(-ends_with(paste0("_", stub_vars)))

  return(data_cube)
}

#' Turn metadata and data cube into px lines
#'
#' @inheritParams get_data_cube
#'
#' @returns A character vector
format_data_as_px_lines <- function(metadata_df, data_df) {
  time_variables <-
    metadata_df %>%
    dplyr::filter(keyword == "TIMEVAL") %>%
    dplyr::pull(variable)

  metadata_lines <-
    metadata_df %>%
    dplyr::left_join(dplyr::select(get_px_keywords(), keyword, quote_value),
                     by = 'keyword'
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(keyword = keyword %>%
                    add_language_to_keyword(get_main_language(metadata_df),
                                            language
                    ) %>%
                    add_sub_key_to_keyword(variable) %>%
                    add_cell_to_keyword(cell)
    ) %>%
    dplyr::mutate(value = value %>%
                    tidyr::replace_na("") %>%
                    paste(collapse = '","') %>%
                    ifelse(quote_value,
                           quote_unless_yes_no(.),
                           .
                    ) %>%
                    break_long_lines(max_line_length = 256) %>%
                    list()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(value) %>%
    dplyr::mutate(repeated_keyword = !is.na(dplyr::lag(keyword)) &
                    keyword == dplyr::lag(keyword),
                  last = is.na(dplyr::lead(repeated_keyword)) |
                    !dplyr::lead(repeated_keyword)
    ) %>%
    dplyr::mutate(line = stringr::str_c(ifelse(repeated_keyword,
                                               "",
                                               paste0(keyword, "=")
    ),
    value,
    ifelse(last, ";", "")
    )
    ) %>%
    dplyr::pull(line)

  data_cube <- get_data_cube(metadata_df, data_df)

  data_lines <-
    data_cube %>%
    mutate_all_vars_to_character() %>%
    dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, '"-"'))) %>%
    tidyr::unite(tmp, sep = " ") %>%
    dplyr::pull(tmp)

  c(metadata_lines, "DATA=", data_lines, ";")
}

get_metadata_df_from_px <- function(px) {
  metadata_template <- dplyr::tibble(keyword  = character(),
                                     language = character(),
                                     variable = character(),
                                     cell     = character(),
                                     value    = list(character())
                                     )

  languages <-
    px$languages %>%
    dplyr::summarise(value = list(paste0(language, sep = ""))) %>%
    dplyr::mutate(keyword = "LANGUAGES") %>%
    dplyr::filter(value != "")

  table <-
    px$table1 %>%
    wrap_varaible_in_list(value)

  table_language_dependent <-
    px$table2 %>%
    # something might be missing here, where codelists is joined, maybe it's
    # related to valuenote which indexes this table?
    # filter on contvariable
    # dplyr::left_join(dplyr::select(px$codelists2, code = value, language,`variable-code`) %>%
    #                    tidyr::drop_na(code),
    #                  by = c("code", "language")
    #                  ) %>%
    dplyr::rename(variable = code) %>%
    wrap_varaible_in_list(value)

  variablecode <-
    px$variables2 %>%
    dplyr::distinct(keyword = "VARIABLECODE",
                    language,
                    variable = `variable-label`,
                    value = `variable-code`
    ) %>%
    wrap_varaible_in_list(value)

  note_etc <-
    px$variables2 %>%
    tidyr::pivot_longer(cols = c(-`variable-code`, -language, -`variable-label`),
                        names_to = "keyword",
                        values_to = "value"
    ) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    dplyr::select(keyword, variable = `variable-label`, language, value) %>%
    dplyr::arrange_all() %>%
    wrap_varaible_in_list(value)

  name_relation <-
    dplyr::select(px$variables2, `variable-code`, language, `variable-label`)

  variables1 <-
    px$variables1 %>%
    dplyr::left_join(name_relation, by = "variable-code")

  head_stub_variable_names <-
    variables1 %>%
    dplyr::filter(toupper(pivot) %in% c("STUB", "HEADING")) %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  head_stub <-
    variables1 %>%
    dplyr::filter(`variable-code` %in% head_stub_variable_names) %>%
    dplyr::mutate(keyword = toupper(pivot)) %>%
    dplyr::arrange(keyword, order) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::summarise(value = list(paste(`variable-label`, sep = ", ")),
                     .groups = "keep"
    )

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
      px$data %>%
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
    px$data %>%
    dplyr::select(dplyr::all_of(head_stub_variable_names)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable-code",
                        values_to = "code"
                        ) %>%
    dplyr::distinct_all() %>%
    dplyr::anti_join(px$codelists2, by = c("variable-code", "code")) %>%
    dplyr::mutate(value = code) %>%
    tidyr::crossing(language = px$languages$language)

  codelists <-
    px$codelists2 %>%
    dplyr::bind_rows(codes_not_in_codelist) %>%
    dplyr::left_join(px$codelists1, by = c("variable-code", "code")) %>%
    dplyr::left_join(name_relation, by = c("variable-code", "language"))

  code_value <-
    codelists %>%
    tidyr::pivot_longer(cols = c("code", "value"), names_to = "type") %>%
    # Update when implementing #140
    dplyr::mutate(keyword = toupper(paste0(type, "s"))) %>%
    dplyr::arrange(keyword, order) %>%
    dplyr::rename(variable = `variable-label`) %>%
    dplyr::group_by(keyword, language, variable) %>%
    dplyr::summarise(value = list(paste(value, sep = ", ")), .groups = "keep") %>%
    dplyr::ungroup()

  valuenote <-
    codelists %>%
    dplyr::mutate(keyword = "VALUENOTE") %>%
    tidyr::drop_na(valuenote) %>%
    wrap_varaible_in_list(valuenote) %>%
    dplyr::select(keyword,
                  language,
                  variable = `variable-label`,
                  cell = value,
                  value = valuenote
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

  metadata_df <-
    dplyr::bind_rows(metadata_template,
                     languages,
                     table,
                     table_language_dependent,
                     variablecode,
                     note_etc,
                     head_stub,
                     timeval,
                     code_value,
                     valuenote,
                     precision
                     ) %>%
    replace_na_language_with_main_language() %>%
    sort_metadata_df()

  return(metadata_df)
}

save_px_as_px_file <- function(px, path) {

  # move metadata_df inside formata_data_as_px
  metadata_df <- get_metadata_df_from_px(px)

  px_lines <- format_data_as_px_lines(metadata_df, px$data)

  write_lines_to_file(px_lines, path, get_encoding_from_metadata(metadata_df))
}

#' Create a px object form a px-file
#'
#' @param path Path to a px-file
#'
#' @return A px object
px_from_px_file <- function(path) {
  px_lines <- read_px_file(path)

  data_line_index <- stringr::str_which(px_lines, '^DATA=$')

  error_if_not_exactly_one_data_line(data_line_index)

  metadata_lines <- px_lines[c(1:data_line_index)]
  data_lines     <- px_lines[c((data_line_index+1):length(px_lines))]

  metadata_df <-
    metadata_lines %>%
    get_metadata_df_from_px_lines() %>%
    add_main_language()

  variable_label <- get_variable_label(metadata_df)

  name_relation <-
    variable_label %>%
    dplyr::distinct(`variable-code`, language, `variable-label`)

  metadata <-
    metadata_df %>%
    dplyr::rename(`variable-label` = variable) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`variable-label` = ifelse(keyword %in% ("CONTVARIABLE"),
                                            unlist(value),
                                            `variable-label`
                                            )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(name_relation, by = c("language", "variable-label")) %>%
    dplyr::left_join(get_px_keywords(), by = "keyword")

  # languages
  languages <-
    metadata %>%
    dplyr::filter(keyword == "LANGUAGES") %>%
    tidyr::unnest(value) %>%
    dplyr::select(language = value) %>%
    align_data_frames(get_base_languages())

  table_sheet_data <-
    metadata %>%
    dplyr::filter(in_table_sheet) %>%
    # Exclude variable specific NOTE
    dplyr::filter(!(keyword == "NOTE" & !is.na(`variable-label`))) %>%
    tidyr::unnest(value)

  # table1
  table1 <-
    table_sheet_data %>%
    dplyr::filter(! language_dependent) %>%
    dplyr::select(keyword, value) %>%
    align_data_frames(get_base_table1())

  # table2
  table2 <-
    table_sheet_data %>%
    dplyr::filter(language_dependent) %>%
    dplyr::select(keyword, code = cell, language, value) %>%
    align_data_frames(get_base_table2())

  # variable1
  figures_variable <-
    variable_label %>%
    dplyr::filter(is.na(keyword)) %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  if (identical(figures_variable, character(0))) {
    figures_variable <- "figures_"
  }

  empty_type_df <- tidyr::tibble(`variable-code` = character(0),
                                 type = character(0)
                                 )

  time_var <-
    metadata %>%
    dplyr::filter(keyword == "TIMEVAL", main_language) %>%
    dplyr::pull(`variable-code`)

  if (identical(time_var, character(0))) {
    time_variable_df <- empty_type_df
  } else {
    time_variable_df <- dplyr::tibble(`variable-code` = time_var, type = "TIME")
  }

  contvariable <-
    metadata %>%
    dplyr::filter(keyword == "CONTVARIABLE", main_language) %>%
    tidyr::unnest(value) %>%
    dplyr::pull(`variable-code`)

  if (identical(contvariable, character(0))) {
    contvariable_df <- empty_type_df
  } else {
    contvariable_df <-
      dplyr::tibble(`variable-code` = contvariable, type = "CONTVARIABLE")
  }

  type_df <- dplyr::bind_rows(time_variable_df, contvariable_df)

  variables1 <-
    variable_label %>%
    dplyr::distinct(`variable-code`, pivot = keyword, order = index) %>%
    dplyr::left_join(type_df, by = "variable-code") %>%
    dplyr::filter(! `variable-code` %in% figures_variable) %>%
    dplyr::bind_rows(dplyr::tibble(`variable-code` = figures_variable,
                                   pivot = "FIGURES"
                                   )
                     )


  # variables2
  variables2 <-
    metadata %>%
    dplyr::filter(keyword %in% c("NOTE", "ELIMINATION", "DOMAIN"),
                  ! is.na(`variable-code`)
                  ) %>%
    dplyr::mutate(keyword = tolower(keyword)) %>%
    dplyr::select(keyword, language, `variable-code`, `variable-label`, value) %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider(names_from = keyword,
                       values_from = value
                       ) %>%
    # Add variables without NOTE, ELIMINATION, DOMAIN to get all variable-labels
    dplyr::bind_rows(dplyr::anti_join(name_relation, .,
                                      by=c("variable-code",
                                           "language",
                                           "variable-label"
                                           )
                                      )
                     ) %>%
    align_data_frames(get_base_variables2())

  # codelist1, codelist2
  codes <-
    metadata %>%
    dplyr::filter(main_language, keyword %in% c("CODES"),
                  !`variable-code` %in% time_var # Time vars should not be in codelist
                  ) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(code = value) %>%
    dplyr::group_by(`variable-code`) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(`variable-code`, code, order)

  precision <-
    metadata %>%
    dplyr::filter(keyword == "PRECISION") %>%
    tidyr::unnest(value) %>%
    dplyr::rename(value = cell, precision = value) %>%
    dplyr::select(`variable-code`, value, precision)

  values <-
    metadata %>%
    dplyr::filter(keyword %in% c("VALUES")) %>%
    tidyr::unnest(value) %>%
    dplyr::group_by(`variable-code`, language) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(`variable-code`),
                                           `variable-label`,
                                           `variable-code`
                                           )
                  ) %>%
    dplyr::select(`variable-code`, value, language, main_language, order)

  valuenote <-
    metadata %>%
    dplyr::filter(keyword %in% c("VALUENOTE")) %>%
    tidyr::unnest(value) %>%
    dplyr::select(`variable-code`, language, value = cell, valuenote = value,
                  language)

  codes_and_values <-
    codes %>%
    # Use values as codes, if codes are missing
    dplyr::full_join(values, by = c("variable-code", "order"),
                     multiple = "all"
                     ) %>%
    dplyr::full_join(valuenote, by = c("variable-code", "value", "language")) %>%
    dplyr::mutate(code = ifelse(is.na(code), value, code))

  codelists <-
    codes_and_values %>%
    dplyr::select(-main_language) %>%
    dplyr::filter(!`variable-code` %in% time_var) %>%
    dplyr::left_join(precision, by = c("variable-code", "value"))

  codelists1 <-
    codelists %>%
    dplyr::distinct(`variable-code`, code, order, precision) %>%
    align_data_frames(get_base_codelists1())

  codelists2 <-
    codelists %>%
    dplyr::select(`variable-code`, code, language, value,`valuenote`) %>%
    align_data_frames(get_base_codelists2())

  # data
  # Order: stub1, stub2, ..., heading1, heading2, ...
  expand_order <-
    variable_label %>%
    dplyr::filter(main_language) %>%
    dplyr::mutate(keyword_order = dplyr::case_when(keyword == "STUB" ~ 1,
                                                   keyword == "HEADING" ~ 2,
                                                   TRUE ~ NA
                                                   )
                  ) %>%
    dplyr::arrange(dplyr::across(c(keyword_order, index))) %>%
    dplyr::mutate(expand_order = dplyr::row_number()) %>%
    dplyr::select(`variable-code`, expand_order)

  heading_vars <-
    variable_label %>%
    dplyr::filter(main_language, keyword == "HEADING") %>%
    dplyr::pull(`variable-code`)

  stub_vars <-
    variable_label %>%
    dplyr::filter(main_language, keyword == "STUB") %>%
    dplyr::pull(`variable-code`)

  if (identical(time_var, character(0))) {
    time_values_df <- tidyr::tibble()
  } else {
    time_values <-
      metadata %>%
      dplyr::filter(keyword %in% c("TIMEVAL"), main_language) %>%
      tidyr::unnest(value) %>%
      dplyr::pull(value) %>%
      get_values_from_time_format()

    time_values_df <- dplyr::tibble(`variable-code` = time_var, code = time_values)
  }

  stub_and_heading_values <-
    codes_and_values %>%
    dplyr::filter(main_language, `variable-code` %in% c(heading_vars, stub_vars)) %>%
    dplyr::bind_rows(time_values_df) %>%
    dplyr::distinct(`variable-code`, code) %>%
    dplyr::group_by(`variable-code`) %>%
    dplyr::summarise(code = list(code)) %>%
    dplyr::left_join(expand_order, by = "variable-code") %>%
    dplyr::arrange(expand_order) %>%
    dplyr::select(`variable-code`, code) %>%
    tibble::deframe()

  figures <-
    data_lines %>%
    stringr::str_replace_all(";", "") %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_subset("^$", negate = TRUE) %>%
    tibble::enframe(name = NULL, value = figures_variable) %>%
    dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

  data_df <-
    do.call(tidyr::expand_grid, stub_and_heading_values) %>%
    dplyr::bind_cols(figures)

  new_px(languages = languages,
         table1 = table1,
         table2 = table2,
         variables1 = variables1,
         variables2 = variables2,
         codelists1 = codelists1,
         codelists2 = codelists2,
         data = data_df
         )
}
