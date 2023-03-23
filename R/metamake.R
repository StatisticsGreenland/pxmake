#' Regular expression to parse header in pxfile
#'
#' @returns Character
get_px_metadata_regex <- function() {
  paste0("(?<keyword>[[:upper:]-]+)",    # Leading keyword
         "(?:\\[)?",                     # Maybe opening language bracket [
         "(?<language>[[:alpha:]_-]+)?", # Maybe language
         "(?:\\])?",                     # Maybe closing language bracket ]
         "(?:\\()?",                     # Maybe opening sub-key parentheses (
         "(?<variable>[^\\(\\),]+)?",    # Maybe sub-key
         "(?:,)?(?<cell>[^\\(\\)]+)?",   # Maybe cell value (used by PRECISION)
         "(?:\\))?",                     # Maybe closing sub-key parentheses )
         "=",                            # definitely =
         "(?<value>[^;]*)",              # Value is everything up to ending ;
         "(?:;$)?"                       # Maybe ;
  )
}

#' Return the main language of the px-file
#'
#' @param df A data frame with table metadata
get_main_language <- function(df) {
  df %>%
    dplyr::filter(keyword == "LANGUAGE") %>%
    dplyr::pull(value) %>%
    unlist()
}

#' Get encoding from CODEPAGE string
#'
#' @param str Character containing CODEPAGE and its values.
#' @param default Character of default encoding to return in no match is found.
#'
#' @returns Character
str_extract_px_encoding <- function(str, default = 'utf-8') {
  encoding <- stringr::str_extract(str, '(?<=CODEPAGE=").+(?=";)')

  if (is.na(encoding)) {
    encoding <- default
  }

  return(encoding)
}

#' Get file encoding listed in px-file
#'
#' The encoding is listed under CODEPAGE. If CODEPAGE isen't given, utf-8 is
#' assumed.
#'
#' @param px_file_path Path to px file
#'
#' @returns Character
get_pxfile_encoding <- function(px_file_path) {
  px_file_path %>%
    readChar(nchars = file.info(.)$size) %>%
    str_extract_px_encoding()
}

#' Create an Excel metadata workbook from a px-file
#'
#' Turn a px-file into an Excel metadata workbook. If pxmake() is run on that
#' workbook it turns back into an equivalent px-file.
#'
#' @param pxfile_path Path to px file
#' @param out_path Path to save xlsx file at
#' @param rds_data_path Path to save data cube as rds file. If NULL the data
#' cube is added in the 'Data' sheet in the Excel metadata workbook.
#' @param overwrite_xlsx Should existing metadata workbook be overwritten?
#'
#' @returns Nothing
#'
#' @export
metamake <- function(pxfile_path,
                     out_path,
                     rds_data_path = NULL,
                     overwrite_xlsx = TRUE) {

  file_connection <- file(pxfile_path, encoding = get_pxfile_encoding(pxfile_path))
  lines <- readLines(con = file_connection)
  close(file_connection)

  ## Split metadata in heading and data cube
  data_line_index <- stringr::str_which(lines, '^DATA=$')

  error_if_not_exactly_one_data_line(data_line_index)

  metadata_lines <- lines[c(1:data_line_index)]
  data_lines     <- lines[c((data_line_index+1):length(lines))]

  tmp_metadata <-
    metadata_lines %>%
    # Remove newlines in file. Use semi-colon as line separator
    paste0(collapse = "") %>%
    stringr::str_split(";") %>%
    unlist() %>%
    stringr::str_match(get_px_metadata_regex()) %>%
    magrittr::extract(,-1) %>% # remove full match column
    dplyr::as_tibble() %>%
    # remove leading and trailing "
    dplyr::mutate(dplyr::across(c(variable, value, cell),
                                ~stringr::str_replace_all(., '^"|"$', '')),
                  value = stringr::str_split(value, '","')
                  ) %>%
    dplyr::mutate(language = tidyr::replace_na(language, get_main_language(.)),
                  main_language = language == get_main_language(.)
                  )

  head_stub <-
    tmp_metadata %>%
    dplyr::filter(keyword %in% c("HEADING", "STUB")) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(long_name = value) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(keyword, language, main_language, long_name, index)

  variable_and_long_name <-
    head_stub %>%
    dplyr::filter(main_language) %>%
    dplyr::select(-language, -main_language) %>%
    dplyr::rename(variable = long_name) %>%
    dplyr::right_join(head_stub,
                      by = c("keyword", "index"),
                      multiple = "all"
                      ) %>%
    dplyr::mutate(position = paste0(substr(keyword, 1, 1), index))

  position <-
    variable_and_long_name %>%
    dplyr::distinct(position, variable)

  name_relation <-
    variable_and_long_name %>%
    dplyr::distinct(variable, language, long_name)

  metadata <-
    tmp_metadata %>%
    dplyr::rename(long_name = variable) %>%
    dplyr::left_join(name_relation, by = c("language", "long_name"))


  ### Make metadata sheet: 'Variables'
  long_name <-
    name_relation %>%
    tidyr::pivot_wider(names_from = language,
                       names_glue = "{language}_long_name",
                       values_from = long_name
    )

  note_elimination_domain <-
    metadata %>%
    dplyr::filter(keyword %in% c("NOTE", "ELIMINATION", "DOMAIN")) %>%
    dplyr::select(keyword, language, variable, value) %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider(names_from = c(language, keyword),
                       names_glue = "{language}_{tolower(keyword)}",
                       values_from = value
                       )

  time_vars <-
    metadata %>%
    dplyr::filter(keyword == "TIMEVAL", main_language) %>%
    dplyr::pull(variable)

  sheet_variables <-
    position %>%
    dplyr::left_join(data.frame(variable = time_vars, type = "time"),
                     by = "variable"
                     ) %>%
    dplyr::left_join(long_name, by = "variable") %>%
    dplyr::left_join(note_elimination_domain, by = "variable") %>%
    dplyr::bind_rows(data.frame(variable = "figures_", type = "figures"))

  ### Make metadata sheet: 'Codelists'
  codes <-
    metadata %>%
    dplyr::filter(main_language, keyword %in% c("CODES"),
                  !variable %in% time_vars #Time vars are not in codelist
                  ) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(code = value) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(variable, code, sortorder)

  precision <-
    metadata %>%
    dplyr::filter(keyword == "PRECISION") %>%
    tidyr::unnest(value) %>%
    dplyr::rename(value = cell, precision = value) %>%
    dplyr::select(variable, value, precision)

  values <-
    metadata %>%
    dplyr::filter(keyword %in% c("VALUES")) %>%
    tidyr::unnest(value) %>%
    dplyr::group_by(variable, language) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::select(variable, value, language, sortorder)

  sheet_codelist <-
    codes %>%
    dplyr::right_join(values, by = c("variable", "sortorder"), multiple = "all") %>%
    dplyr::left_join(precision, by = c("variable", "value")) %>%
    tidyr::drop_na(code) %>%
    tidyr::pivot_wider(names_from = language, names_glue = "{language}_code_label") %>%
    dplyr::relocate(precision, .after = last_col())

  ### Make metadata sheet: 'Table'
  sheet_table <-
    metadata %>%
    dplyr::left_join(get_px_keywords(), by = "keyword") %>%
    dplyr::filter(in_table_sheet) %>%
    # Exclude variable specific NOTE
    dplyr::filter(!(keyword == "NOTE" & !is.na(variable))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = paste(value, collapse = '","'),
                  keyword = ifelse(language_dependent,
                                   paste0(keyword, "_", language),
                                   keyword
                                   )
                  ) %>%
    dplyr::select(keyword, value)

  ### Make metadata sheet: 'Data'
  heading_vars <-
    metadata %>%
    dplyr::filter(main_language, keyword == "HEADING") %>%
    dplyr::pull(value) %>%
    unlist()

  stub_vars <-
    metadata %>%
    dplyr::filter(main_language, keyword == "STUB") %>%
    dplyr::pull(value) %>%
    unlist()

  # Order: s1, s2, ..., h1, h2, ...
  expand_order <-
    head_stub %>%
    dplyr::filter(main_language) %>%
    dplyr::mutate(keyword_order = dplyr::case_when(keyword == "STUB" ~ 1,
                                                   keyword == "HEADING" ~ 2,
                                                   TRUE ~ NA
                                                   )
                  ) %>%
    dplyr::arrange(dplyr::across(c(keyword_order, index))) %>%
    dplyr::mutate(expand_order = dplyr::row_number()) %>%
    dplyr::select(long_name, expand_order)

  stub_and_heading_values <-
    metadata %>%
    dplyr::filter(main_language, keyword == "CODES",
                  variable %in% c(heading_vars, stub_vars)
                  ) %>%
    dplyr::left_join(expand_order, by = "long_name") %>%
    dplyr::arrange(expand_order) %>%
    dplyr::select(variable, value) %>%
    tibble::deframe()

  figures <-
    data_lines %>%
    stringr::str_replace_all(";", "") %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_subset("^$", negate = TRUE) %>%
    data.frame(figures_ = .)

  sheet_data <-
    do.call(tidyr::expand_grid, stub_and_heading_values) %>%
    dplyr::bind_cols(figures)

  ### Make sheets in workbook
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(df, sheet_name) {
    openxlsx::addWorksheet(wb,sheet_name, gridLines = FALSE)
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = 'auto')
    openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleMedium2")
  }

  add_sheet(sheet_table,     "Table")
  add_sheet(sheet_variables, "Variables")
  add_sheet(sheet_codelist,  "Codelists")

  if (is.null(rds_data_path)) {
    error_if_too_many_rows_for_excel(sheet_data)

    add_sheet(sheet_data, "Data")
  } else {
    saveRDS(sheet_data, rds_data_path)
  }

  openxlsx::saveWorkbook(wb, out_path, overwrite = overwrite_xlsx)
}
