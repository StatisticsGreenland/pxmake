#' Get data frame from data table argument
#'
#' @inheritParams get_figures_variable
#' @inheritParams get_data_table_df
#'
#' @returns Data frame
get_df_from_data_table_argument <- function(excel_metadata_path, data_table) {
  if (is.null(data_table)) {
    return(get_data_sheet(excel_metadata_path))
  } else if (is.data.frame(data_table)) {
    return(data_table)
  } else if (file.exists(data_table)) {
    # try catch
    return(readRDS(data_table))
  } else {
    # error
  }
}

#' Get table data as data frame
#'
#' @inheritParams get_figures_variable
#' @param data_table A data frame or a path to an `.rds` file with data source.
#' If NULL, the data is taken from the sheet 'Data' in the Excel metadata
#' workbook.
#' @param add_totals A list of variables to add a 'total' level to. The value
#' of the total level is looked up in 'Variables' xx_elimination. The code for
#' the level is found in 'Codelists'. The total is a sum of the values in the
#' variables with type = FIGURES in 'Variables'. NAs are ignored when summing.
#'
#' @returns a data frame
get_data_table_df  <- function(excel_metadata_path, data_table, add_totals) {
  data_table_df <-
    get_df_from_data_table_argument(excel_metadata_path, data_table) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(-one_of(get_figures_variable(excel_metadata_path)),
                         as.character
                         )
                  )

  if (is.null(add_totals)) {
    return(data_table_df)
  }

  variables <-
    get_variables_metadata(excel_metadata_path) %>%
    dplyr::select(variable, language, elimination)

  codelist <-
    get_codelists_metadata(excel_metadata_path, data_table_df) %>%
    dplyr::select(variable, code, value)

  params <-
    variables %>%
    dplyr::left_join(codelist,
                     by = c("variable", "elimination" = "value"),
                     multiple = "all"
                     ) %>%
    dplyr::filter(variable %in% add_totals) %>%
    dplyr::distinct(variable, code)

  add_totals(data_table_df,
             vars = params$variable,
             level_names = params$code,
             sum_var = get_figures_variable(excel_metadata_path)
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

#' Get the main language from metadata
#'
#' @inheritParams sort_metadata_df
#'
#' @returns Character
get_main_language <- function(metadata_df) {
  metadata_df %>%
    dplyr::filter(keyword == "LANGUAGE") %>%
    tidyr::unnest(value) %>%
    dplyr::pull(value)
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
    dplyr::arrange(order, keyword, language_order, !is.na(variable), variable, cell) %>%
    dplyr::select(-order, -language_order)
}

#' Get a data frame with metadata
#'
#' The metadata from the 3 sheets in the Excel workbook are combined into one
#' data frame, in a format that's both easy to read, query and convert to a
#' px-file.
#'
#' @inheritParams get_figures_variable
#' @param data_table_df Data frame with data table in tidy format.
#'
#' @returns A data frame
get_metadata_df <- function(excel_metadata_path, data_table_df) {
  df <- dplyr::tibble(keyword  = character(),
                      language = character(),
                      variable = character(),
                      cell     = character(),
                      value    = list(character())
                      )

  variables_long <-
    excel_metadata_path %>%
    get_variables_metadata() %>%
    tidyr::pivot_longer(cols = -c(position, variable, type, language, long_name),
                        names_to = "keyword"
                        )

  note_etc <-
    variables_long %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(keyword = toupper(keyword)) %>%
    wrap_varaible_in_list(value) %>%
    dplyr::select(keyword, variable = long_name, language, value) %>%
    dplyr::arrange_all()

  variables_long_distinct <-
    variables_long %>%
    tidyr::drop_na(position) %>%
    dplyr::distinct(position, variable, language, long_name)

  variablecode <-
    variables_long_distinct %>%
    dplyr::distinct(keyword = "VARIABLECODE",
                    value = variable,
                    variable = long_name,
                    language
                    ) %>%
    wrap_varaible_in_list(value)

  # Simplify when #124 is implemented.
  head_stub <-
    variables_long_distinct %>%
    # Simplify when #124 is implemented.
    dplyr::mutate(type     = substr(position, 1, 1) %>% tolower(),
                  order    = substr(position, 2, nchar(position)),
                  keyword  = dplyr::case_when(type == 's' ~ 'STUB',
                                              type == 'h' ~ 'HEADING',
                                              TRUE ~ NA_character_
                                              )
                  ) %>%
    dplyr::arrange(keyword, order) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::summarise(value = list(paste(long_name, sep = ", ")), .groups = "keep")

  time_metadata <-
    variables_long %>%
    dplyr::filter(tolower(type) == "time") %>%
    dplyr::distinct(variable, language, long_name)

  time_variable <-
    time_metadata %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)

  if (length(time_variable) == 0) {
    timeval <- NULL
  } else {
    error_if_more_than_one_time_variable(time_variable)

    time_values <-
      data_table_df %>%
      dplyr::distinct(across(all_of(time_variable))) %>%
      dplyr::pull(1)

    timeval <-
      time_metadata %>%
      dplyr::mutate(keyword = "TIMEVAL",
                    value = paste0("TLIST(",
                                   get_timeval_type_from_values(time_values),
                                   "1),",
                                   time_values %>%
                                     stringr::str_replace_all('[:alpha:]', '') %>%
                                     str_quote() %>%
                                     stringr::str_c(collapse = ',')
                                   )
                    ) %>%
      dplyr::select(keyword, language, variable = long_name, value) %>%
      wrap_varaible_in_list(value)
  }

  codelists <- get_codelists_metadata(excel_metadata_path, data_table_df)

  code_value <-
    codelists %>%
    tidyr::pivot_longer(cols = c("code",  "value"), names_to = "type") %>%
    # Update when implementing #140
    dplyr::mutate(keyword = toupper(paste0(type, "s"))) %>%
    dplyr::arrange(keyword, sortorder) %>%
    dplyr::group_by(keyword, language, variable = long_name) %>%
    dplyr::summarise(value = list(paste(value, sep = ", ")), .groups = "keep") %>%
    dplyr::ungroup()

  precision <-
    codelists %>%
    dplyr::mutate(keyword = "PRECISION") %>%
    tidyr::drop_na(precision) %>%
    wrap_varaible_in_list(precision) %>%
    dplyr::select(keyword,
                  language,
                  variable = long_name,
                  cell = value,
                  value = precision
                  )

  dplyr::bind_rows(df,
                   get_table_metadata(excel_metadata_path),
                   variablecode,
                   note_etc,
                   head_stub,
                   timeval,
                   code_value,
                   precision
                   ) %>%
    sort_metadata_df()
}

#' Get data cube used in px file format
#'
#' @inheritParams sort_metadata_df
#' @inheritParams get_metadata_df
#'
#' @returns Data frame
get_data_cube <- function(metadata_df, data_table_df, excel_metadata_path) {
  metadata_df_main_language <-
    metadata_df %>%
    dplyr::filter(language == get_main_language(metadata_df))

  long_names <-
    metadata_df_main_language %>%
    dplyr::filter(keyword == "VARIABLECODE") %>%
    tidyr::unnest(value) %>%
    dplyr::select(long_name = variable, variable = value)

  stub_and_heading_df <-
    metadata_df_main_language %>%
    dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
    tidyr::unnest(value) %>%
    dplyr::select(keyword, long_name = value) %>%
    dplyr::left_join(long_names, by = "long_name")

  stub_vars <-
    stub_and_heading_df %>%
    dplyr::filter(keyword == "STUB") %>%
    dplyr::pull(variable)

  heading_vars <-
    stub_and_heading_df %>%
    dplyr::filter(keyword == "HEADING") %>%
    dplyr::pull(variable)

  head_stub_variable_names <- c(heading_vars, stub_vars)

  codelist <-
    get_codelists_metadata(excel_metadata_path, data_table_df) %>%
    dplyr::filter(language == get_main_language(metadata_df)) %>%
    dplyr::select(variable, sortorder, code)

  # Complete data by adding all combinations of variable values in data and
  # codelist
  source_data_values <-
    data_table_df %>%
    dplyr::select(dplyr::all_of(head_stub_variable_names)) %>%
    lst_distinct_and_arrange()

  codelist_values <-
    split(codelist$code, codelist$variable) %>%
    lst_distinct_and_arrange()

  data_values <- merge_named_lists(source_data_values, codelist_values)

  data_cube <-
    data_table_df %>%
    tidyr::complete(!!!data_values) %>%
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
    dplyr::arrange(dplyr::across(zip_vectors(paste0("sortorder_", heading_vars),
                                             paste0("code_", heading_vars)
                                             )
                                 )
                   ) %>%
    dplyr::select(-paste0("sortorder_", heading_vars)) %>%
    tidyr::pivot_wider(names_from = !!paste0("code_", heading_vars),
                       values_from = get_figures_variable(excel_metadata_path)
                       ) %>%
    dplyr::arrange(dplyr::across(zip_vectors(paste0("sortorder_", stub_vars),
                                             paste0("code_", stub_vars)
                                             )
                                 )
                   ) %>%
    dplyr::select(-ends_with(paste0("_", stub_vars)))

  return(data_cube)
}

#' Turn metadata and data cube into px lines
#'
#' @inheritParams sort_metadata_df
#' @param data_cube Data frame
#'
#' @returns A character vector
format_data_as_px_lines <- function(metadata_df, data_cube) {
  time_variables <-
    metadata_df %>%
    dplyr::filter(keyword == "TIMEVAL") %>%
    dplyr::pull(variable)

  metadata_lines <-
    metadata_df %>%
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
                            quote_unless_numeric_or_yes_no() %>%
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
    data_cube %>%
    dplyr::mutate(dplyr::across(everything(), as.character),
                  dplyr::across(everything(), ~tidyr::replace_na(.x, '"-"'))
                  ) %>%
    tidyr::unite(tmp, sep = " ") %>%
    dplyr::pull(tmp)

  c(metadata_lines, "DATA=", data_lines, ";")
}

#' Write lines to file
#'
#' @param lines Character vector
#' @param path Path to save file at
#' @param encoding File encoding
write_lines_to_file <- function(lines, path, encoding) {
  file_connection <- file(path, encoding = encoding)
  writeLines(lines, file_connection)
  close(file_connection)
}

#' Create a px-file from an Excel metadata workbook and a data table
#'
#' @param px_path Path to save px file at
#' @inheritParams get_data_table_df
#'
#' @returns Nothing
#'
#' @export
pxmake <- function(excel_metadata_path,
                   px_path,
                   data_table = NULL,
                   add_totals = NULL) {

  data_table_df  <- get_data_table_df(excel_metadata_path, data_table, add_totals)
  metadata_df    <- get_metadata_df(excel_metadata_path, data_table_df)

  data_cube <- get_data_cube(metadata_df, data_table_df, excel_metadata_path)

  px_lines <- format_data_as_px_lines(metadata_df, data_cube)

  write_lines_to_file(px_lines, px_path, get_encoding_from_metadata(metadata_df))
}
