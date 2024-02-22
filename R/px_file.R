#' Get data cube used in px file format
#'
#' @inheritParams sort_metadata_df
#' @inheritParams format_data_df
#'
#' @return A data frame
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
    mutate_all_vars_to_character() %>%
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
    mutate_all_vars_to_character() %>%
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

#' Get lines for px-file from px object
#'
#' @param x A px object
#'
#' @return A character vector
format_px_object_as_lines <- function(x) {
  metadata_df <- get_metadata_df_from_px(x)

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

  data_lines <-
    get_data_cube(metadata_df, x$data) %>%
    mutate_all_vars_to_character() %>%
    dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, '"-"'))) %>%
    tidyr::unite(tmp, sep = " ") %>%
    dplyr::pull(tmp)

  c(metadata_lines, "DATA=", data_lines, ";")
}

#' Save px object to px-file
#'
#' @param x A px object
#' @param path Path to save px-file at
#'
#' @return Nothing
save_px_as_px_file <- function(x, path) {
  px_lines <- format_px_object_as_lines(x)

  encoding_str <-
    x$table1 %>%
    dplyr::filter(keyword == "CODEPAGE") %>%
    dplyr::pull(value)

  if (length(encoding_str) == 0) {
    encoding_str <- get_default_encoding()
  }

  file_connection <- file(path, encoding = encoding_str)
  writeLines(px_lines, file_connection)
  close(file_connection)
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

    name_relation <-
      name_relation %>%
      dplyr::bind_rows(tidyr::crossing(`variable-code` = figures_variable,
                                       `variable-label` = figures_variable,
                                       language = unique(name_relation$language)
                                       )
                       )
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
    dplyr::filter(main_language,
                  `variable-code` %in% c(heading_vars, stub_vars),
                  # Exclude time variable as it is already in time_values_df
                  ! `variable-code` %in% time_var
                  ) %>%
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
    dplyr::bind_cols(figures) %>%
    dplyr::as_tibble()

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
