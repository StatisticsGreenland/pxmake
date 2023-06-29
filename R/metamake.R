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
         "(?<cell>[^\"]+)?",             # Maybe cell value (used by PRECISION)
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
get_metdata_df_from_px_lines <- function(metadata_lines) {
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
    # remove double quotes caused by collapsing values spanning multiple lines
    dplyr::mutate(value = stringr::str_replace_all(value, '""', '')) %>%
    dplyr::mutate(value = ifelse(keyword != "TIMEVAL",
                                 stringr::str_split(value, '","'),
                                 value
                                 )
                  ) %>%
    dplyr::filter(keyword != "DATA")
}

#' Create an Excel metadata workbook from a px-file
#'
#' @param input Input can be provided in one of three ways:
#' 1. A path to a `.px` file.
#' 1. A path to a `.rds` file created by \link{pxmake}.
#' 1. A named list with two data frames "metadata" and "data" (same as option 2).
#' @param out_path Path to save metadata at. Use `.xlsx` extension to save
#' as an Excel workbook. Use `.rds` to save as an rds file. If NULL, no file is
#' saved.
#' @param data_path Path to save data as an .rds file. If NULL, the data is
#' saved as the sheet 'Data' in the Excel metadata workbook.
#'
#' @returns Returns rds object invisibly.
#'
#' @seealso \link{pxmake}
#'
#' @export
metamake <- function(input, out_path = NULL, data_path = NULL) {
  validate_metamake_arguments(input, out_path, data_path)

  if (is_rds_file(input)) {
    input <- readRDS(input)
  }

  if (is_rds_list(input)) {
    metadata_df <- input$metadata
  } else if (is_px_file(input)) {
    px_lines <- read_px_file(input)

    data_line_index <- stringr::str_which(px_lines, '^DATA=$')

    error_if_not_exactly_one_data_line(data_line_index)

    metadata_lines <- px_lines[c(1:data_line_index)]
    data_lines     <- px_lines[c((data_line_index+1):length(px_lines))]

    metadata_df <- get_metdata_df_from_px_lines(metadata_lines)
  } else {
    unexpected_error()
  }

  variable_and_long_name <- get_variable_long_names(metadata_df)

  metadata_df <- add_main_language(metadata_df)

  heading_vars <-
    variable_and_long_name %>%
    dplyr::filter(main_language, keyword == "HEADING") %>%
    dplyr::pull(variable)

  stub_vars <-
    variable_and_long_name %>%
    dplyr::filter(main_language, keyword == "STUB") %>%
    dplyr::pull(variable)

  pivot_order <-
    variable_and_long_name %>%
    dplyr::filter(main_language, !is.na(keyword)) %>%
    dplyr::distinct(pivot = keyword,
                    order = index,
                    variable
                    )

  name_relation <-
    variable_and_long_name %>%
    dplyr::distinct(variable, language, long_name)

  figures_var <-
    variable_and_long_name %>%
    dplyr::filter(is.na(keyword)) %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)

  if (identical(figures_var, character(0))) {
    figures_var <- "figures_"
  }

  metadata <-
    metadata_df %>%
    dplyr::rename(long_name = variable) %>%
    dplyr::left_join(name_relation, by = c("language", "long_name"))

  ### Make metadata sheet: 'Variables'
  long_name <-
    name_relation %>%
    tidyr::drop_na(language) %>%
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

  time_var <-
    metadata %>%
    dplyr::filter(keyword == "TIMEVAL", main_language) %>%
    dplyr::pull(variable)

  if (identical(time_var, character(0))) {
    time_variable_df <- data.frame(variable = character(0), type = character(0))
  } else {
    time_variable_df <- data.frame(variable = time_var, type = "time")
  }

  sheet_variables <-
    pivot_order %>%
    dplyr::left_join(time_variable_df, by = c("variable")) %>%
    dplyr::bind_rows(data.frame(variable = figures_var, pivot = "FIGURES")) %>%
    dplyr::left_join(long_name, by = "variable") %>%
    dplyr::left_join(note_elimination_domain, by = "variable")

  ### Make metadata sheet: 'Codelists'
  codes <-
    metadata %>%
    dplyr::filter(main_language, keyword %in% c("CODES"),
                  !variable %in% time_variable_df #Time vars are not in codelist
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
    dplyr::select(variable, value, language, main_language, sortorder)

  codes_and_values <-
    codes %>%
    # Use values as codes, if codes are missing
    dplyr::full_join(values, by = c("variable", "sortorder"), multiple = "all") %>%
    dplyr::mutate(code = ifelse(is.na(code), value, code))

  sheet_codelist <-
    codes_and_values %>%
    dplyr::select(-main_language) %>%
    dplyr::filter(!variable %in% time_var) %>%
    dplyr::left_join(precision, by = c("variable", "value")) %>%
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
    dplyr::mutate(value = paste(value, collapse = ','),
                  keyword = ifelse(language_dependent,
                                   paste0(keyword, "_", language),
                                   keyword
                  )
                  ) %>%
    dplyr::select(keyword, value)

  ### Make metadata sheet: 'Data'

  # Order: s1, s2, ..., h1, h2, ...
  expand_order <-
    variable_and_long_name %>%
    dplyr::filter(main_language) %>%
    dplyr::mutate(keyword_order = dplyr::case_when(keyword == "STUB" ~ 1,
                                                   keyword == "HEADING" ~ 2,
                                                   TRUE ~ NA
                                                   )
                  ) %>%
    dplyr::arrange(dplyr::across(c(keyword_order, index))) %>%
    dplyr::mutate(expand_order = dplyr::row_number()) %>%
    dplyr::select(variable, expand_order)

  stub_and_heading_values <-
    codes_and_values %>%
    dplyr::filter(main_language, variable %in% c(heading_vars, stub_vars)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(code = list(code)) %>%
    dplyr::left_join(expand_order, by = "variable") %>%
    dplyr::arrange(expand_order) %>%
    dplyr::select(variable, code) %>%
    tibble::deframe()

  if (is_rds_list(input)) {
    sheet_data <-
      do.call(tidyr::expand_grid, stub_and_heading_values) %>%
      dplyr::left_join(input$data, by = c(stub_vars, heading_vars))
  } else {
    figures <-
      data_lines %>%
      stringr::str_replace_all(";", "") %>%
      stringr::str_split(" ") %>%
      unlist() %>%
      stringr::str_subset("^$", negate = TRUE) %>%
      tibble::enframe(name = NULL, value = figures_var) %>%
      dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

    sheet_data <-
      do.call(tidyr::expand_grid, stub_and_heading_values) %>%
      dplyr::bind_cols(figures)
  }

  rds <- list("metadata" = dplyr::select(metadata_df, -main_language),
              "data" = sheet_data
              )

  if (is_rds_file(out_path)) {
    saveRDS(rds, out_path)
  } else if (is_xlsx_file(out_path)) {
    ### Make sheets in workbook
    wb <- openxlsx::createWorkbook()

    add_sheet <- function(df, sheet_name) {
      openxlsx::addWorksheet(wb,sheet_name, gridLines = FALSE)
      openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = 'auto')
      openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleLight9")
    }

    add_sheet(sheet_table,     "Table")
    add_sheet(sheet_variables, "Variables")
    add_sheet(sheet_codelist,  "Codelists")

    if (is.null(data_path)) {
      error_if_too_many_rows_for_excel(sheet_data)
      add_sheet(sheet_data, "Data")
    } else if (tools::file_ext(data_path) == "rds") {
      saveRDS(sheet_data, data_path)
    } else {
      unexpected_error()
    }
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  } else if (is.null(out_path)) {
    # pass
  } else {
    unexpected_error()
  }

  invisible(rds)
}
