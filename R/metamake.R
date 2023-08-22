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

#' Create a minimal metadata template
#'
#' @param data_df A data frame with data to create metadata for
#'
#' @returns A data frame
get_metadata_template_from_data <- function(data_df) {
  variable_names <- names(data_df)

  heading_variables <- head(variable_names, 1)
  stub_variables    <- head(tail(variable_names, -1), -1)
  figures_variable  <- tail(variable_names, 1)

  values <-
    data_df %>%
    dplyr::select(-all_of(figures_variable)) %>%
    mutate_all_vars_to_character() %>%
    tidyr::pivot_longer(cols = everything(), names_to = "variable") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(value = list(unique(value)))

  get_px_keywords() %>%
    dplyr::filter(mandatory,
                  ! keyword %in% c("DATA", "STUB", "HEADING", "VALUES", "DECIMALS")
                  ) %>%
    dplyr::select(keyword) %>%
    dplyr::bind_rows(dplyr::tibble(keyword = c("NOTE", "ELIMINATION", "DOMAIN"))) %>%
    dplyr::mutate(value = list("")) %>%
    dplyr::bind_rows(
      dplyr::tribble(~keyword,             ~value,
                     "STUB",       stub_variables,
                     "HEADING", heading_variables,
                     "DECIMALS",              "0",
                     "LANGUAGE",             "en"
                     ) %>%
        wrap_varaible_in_list(value),
      dplyr::tibble(keyword = "VALUES", values),
      dplyr::tibble(keyword = "VARIABLECODE",
                    variable = figures_variable,
                    value = list(figures_variable)
                    )
      ) %>%
    dplyr::mutate(language = "en", cell = NA_character_) %>%
    dplyr::relocate(value, .after = last_col())
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

#' Create an Excel metadata workbook from a px-file
#'
#' @param input Input can be provided in one of four ways:
#' 1. A path to a `.px` file.
#' 1. A path to a `.rds` file created by \link{pxmake}.
#' 1. A named list with two data frames "metadata" and "data" (same as option 2).
#' 1. A data frame with data. A minimal metadata template will be created.
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
  } else if (is.data.frame(input)) {
    data_df <- input
    input <- list("metadata" = get_metadata_template_from_data(data_df),
                  "data" = data_df
                  )
  }

  if (is_rds_list(input)) {
    metadata_df <- input$metadata
  } else if (is_px_file(input)) {
    px_lines <- read_px_file(input)

    data_line_index <- stringr::str_which(px_lines, '^DATA=$')

    error_if_not_exactly_one_data_line(data_line_index)

    metadata_lines <- px_lines[c(1:data_line_index)]
    data_lines     <- px_lines[c((data_line_index+1):length(px_lines))]

    metadata_df <- get_metadata_df_from_px_lines(metadata_lines)
  } else {
    unexpected_error()
  }

  variable_label <- get_variable_label(metadata_df)

  metadata_df <- add_main_language(metadata_df)

  heading_vars <-
    variable_label %>%
    dplyr::filter(main_language, keyword == "HEADING") %>%
    dplyr::pull(`variable-code`)

  stub_vars <-
    variable_label %>%
    dplyr::filter(main_language, keyword == "STUB") %>%
    dplyr::pull(`variable-code`)

  pivot_order <-
    variable_label %>%
    dplyr::filter(main_language, !is.na(keyword)) %>%
    dplyr::distinct(pivot = keyword,
                    order = index,
                    `variable-code`
                    )

  name_relation <-
    variable_label %>%
    dplyr::distinct(`variable-code`, language, `variable-label`)

  figures_var <-
    variable_label %>%
    dplyr::filter(is.na(keyword)) %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  if (identical(figures_var, character(0))) {
    figures_var <- "figures_"
  }

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
    dplyr::left_join(name_relation, by = c("language", "variable-label"))

  ### Make metadata sheet: 'Variables'
  variable_label_wide <-
    name_relation %>%
    tidyr::drop_na(language) %>%
    tidyr::pivot_wider(names_from = language,
                       names_glue = "{language}_variable-label",
                       values_from = `variable-label`
                       )

  note_elimination_domain <-
    metadata %>%
    dplyr::filter(keyword %in% c("NOTE", "ELIMINATION", "DOMAIN")) %>%
    dplyr::select(keyword, language, `variable-code`, value) %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider(names_from = c(language, keyword),
                       names_glue = "{language}_{tolower(keyword)}",
                       values_from = value
                       )


  # Make type df
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
    contvariable_df <- dplyr::tibble(`variable-code` = contvariable, type = "CONTVARIABLE")
  }

  type_df <- dplyr::bind_rows(time_variable_df, contvariable_df)

  sheet_variables <-
    pivot_order %>%
    dplyr::left_join(type_df, by = "variable-code") %>%
    dplyr::bind_rows(dplyr::tibble(`variable-code` = figures_var,
                                   pivot = "FIGURES"
                                   )
                     ) %>%
    dplyr::left_join(variable_label_wide, by = "variable-code") %>%
    dplyr::left_join(note_elimination_domain, by = "variable-code")

  ### Make metadata sheet: 'Codelists'
  codes <-
    metadata %>%
    dplyr::filter(main_language, keyword %in% c("CODES"),
                  !`variable-code` %in% time_variable_df #Time vars are not in codelist
                  ) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(code = value) %>%
    dplyr::group_by(`variable-code`) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(`variable-code`, code, sortorder)

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
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(`variable-code`),
                                           `variable-label`,
                                           `variable-code`
                                           )
                  ) %>%
    dplyr::select(`variable-code`, value, language, main_language, sortorder)

  codes_and_values <-
    codes %>%
    # Use values as codes, if codes are missing
    dplyr::full_join(values, by = c("variable-code", "sortorder"),
                     multiple = "all"
                     ) %>%
    dplyr::mutate(code = ifelse(is.na(code), value, code))

  sheet_codelist <-
    codes_and_values %>%
    dplyr::select(-main_language) %>%
    dplyr::filter(!`variable-code` %in% time_var) %>%
    dplyr::left_join(precision, by = c("variable-code", "value")) %>%
    tidyr::pivot_wider(names_from = language, names_glue = "{language}_code-label") %>%
    dplyr::relocate(precision, .after = last_col())

  ### Make metadata sheet: 'Table'
  table_sheet_data <-
    metadata %>%
    dplyr::left_join(get_px_keywords(), by = "keyword") %>%
    dplyr::filter(in_table_sheet) %>%
    # Exclude variable specific NOTE
    dplyr::filter(!(keyword == "NOTE" & !is.na(`variable-code`))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = paste(value, collapse = ',')) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(order)

  sheet_table <-
    table_sheet_data %>%
    dplyr::filter(!language_dependent) %>%
    dplyr::select(keyword, value)

  contvariable_codes <-
    dplyr::filter(codes_and_values, `variable-code` %in% contvariable) %>%
    dplyr::select(code, value, language)

  sheet_table2 <-
    table_sheet_data %>%
    dplyr::filter(language_dependent) %>%
    dplyr::left_join(contvariable_codes, by = c("cell" = "value", "language")) %>%
    dplyr::select(keyword, code, language, value) %>%
    tidyr::pivot_wider(names_from = language,
                       names_glue = "{language}_value",
                       values_from = "value"
                       )

  ### Make metadata sheet: 'Data'

  # Order: s1, s2, ..., h1, h2, ...
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

  stub_and_heading_values <-
    codes_and_values %>%
    dplyr::filter(main_language, `variable-code` %in% c(heading_vars, stub_vars)) %>%
    dplyr::group_by(`variable-code`) %>%
    dplyr::summarise(code = list(code)) %>%
    dplyr::left_join(expand_order, by = "variable-code") %>%
    dplyr::arrange(expand_order) %>%
    dplyr::select(`variable-code`, code) %>%
    tibble::deframe()

  if (is_rds_list(input)) {
    sheet_data <-
      do.call(tidyr::expand_grid, stub_and_heading_values) %>%
      dplyr::left_join(input$data %>%
                       dplyr::mutate(input$data,
                                     dplyr::across(-one_of(get_figures_variable(input, .)),
                                                   as.character
                                                   )
                                     ),
                       by = c(stub_vars, heading_vars)
                       )
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
    add_sheet(sheet_table2,    "Table2")
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
