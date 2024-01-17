#' Turn Excel workbook into a px object
#'
#' @param excel_path Path to Excel metadata workbook.
#'
#' @return A px object.
px_from_excel <- function(excel_path, data = NULL) {
  # languages, table1
  table_sheet <-
    get_table_sheet(excel_path) %>%
    dplyr::filter(!is.na(keyword))

  languages <-
    table_sheet %>%
    dplyr::filter(keyword %in% c("LANGUAGES")) %>%
    dplyr::mutate(value =  stringr::str_replace_all(value, " ", "") %>%
                    # remove quotes to be backwards compatible
                    stringr::str_replace_all('"', '') %>%
                    stringr::str_split(pattern = ',')
                  ) %>%
    tidyr::unnest(value) %>%
    tidyr::drop_na(value) %>%
    dplyr::select(language = value) %>%
    align_data_frames(get_base_languages())

  table1 <-
    table_sheet %>%
    align_data_frames(get_base_table1()) %>%
    dplyr::filter(! keyword %in% c("LANGUAGES"))

  # table2
  table2 <-
    excel_path %>%
    get_table2_sheet() %>%
    dplyr::filter(!is.na(keyword)) %>%
    tidyr::pivot_longer(cols = ends_with("_value"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        ) %>%
    align_data_frames(get_base_table2())

  # variables1, variables2
  variables_sheet <-
    excel_path %>%
    get_variables_sheet()

  variables1 <-
    variables_sheet %>%
    align_data_frames(get_base_variables1()) %>%
    dplyr::select(`variable-code`, pivot, order, type)

  variables2 <-
    variables_sheet %>%
    dplyr::select(-pivot, -order, -type) %>%
    tidyr::pivot_longer(cols = -c(`variable-code`),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    align_data_frames(get_base_variables2()) %>%
    dplyr::mutate(`variable-label` = ifelse(is.na(`variable-label`),
                                            `variable-code`,
                                            `variable-label`
                                            )
                  )

  # codelists1, codelists2
  raw_data <-
    if (is.null(data)) {
      get_data_sheet(excel_path)
    } else {
      data
    }

  data_df <-
    raw_data %>%
    format_data_df(figures_variable = get_figures_variable_from_excel(excel_path))

  codelists_sheet <-
    excel_path %>%
    get_codelists_sheet()

  codelists1 <-
    codelists_sheet %>%
    align_data_frames(get_base_codelists1()) %>%
    dplyr::select(`variable-code`, code, order = sortorder, precision)

  codelists2 <-
    codelists_sheet %>%
    dplyr::select(-sortorder, -precision) %>%
    tidyr::pivot_longer(cols = ends_with(c("_code-label", "_valuenote")),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    dplyr::rename(value = `code-label`) %>%
    align_data_frames(get_base_codelists2())

  # data_codes <-
  #   data_df %>%
  #   dplyr::select(-any_of(get_figures_variable(excel_metadata_path))) %>%
  #   tidyr::pivot_longer(cols = everything(),
  #                       names_to = "variable-code",
  #                       values_to = "code"
  #                       ) %>%
  #   dplyr::distinct() %>%
  #   tidyr::expand_grid(language = get_all_languages(excel_metadata_path)) %>%
  #   dplyr::arrange_all()
  #
  # variables <-
  #   get_variables_metadata(excel_metadata_path) %>%
  #   dplyr::select(`variable-code`, language, `variable-label`)
  #
  # codelists <-
  #   excel_metadata_path %>%
  #   get_codelists_sheet() %>%
  #   dplyr::mutate(across(-sortorder, as.character),
  #                 across( sortorder, as.numeric)
  #   ) %>%
  #   tidyr::pivot_longer(cols = ends_with(c("_code-label", "_valuenote")),
  #                       names_to = c("language", "keyword"),
  #                       names_pattern = "^([[:alpha:]]+)_(.*)$"
  #   ) %>%
  #   tidyr::pivot_wider(names_from = "keyword") %>%
  #   # Add valuenote if it doesn't exist
  #   dplyr::bind_rows(dplyr::tibble(valuenote = character())) %>%
  #   dplyr::rename(value = `code-label`) %>%
  #   dplyr::full_join(data_codes, by = c("variable-code", "code", "language")) %>%
  #   dplyr::left_join(variables, by = c("variable-code", "language")) %>%
  #   dplyr::mutate(value = ifelse(is.na(value), code, value))

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

save_px_as_xlsx <- function(px, path) {
  excel_table <-
    data.frame(keyword ="LANGUAGES",
               value = paste0(px$languages$language, collapse = ",")
               ) %>%
    tidyr::drop_na(value) %>%
    dplyr::bind_rows(px$table1) %>%
    dplyr::arrange(keyword)


  excel_table2 <-
    px$table2 %>%
    tidyr::pivot_wider(names_from = "language",
                       values_from = "value",
                       names_glue = "{language}_value"
                       )

  excel_variables <-
    px$variables2 %>%
    tidyr::pivot_longer(cols = -c(`variable-code`, `language`),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    tidyr::pivot_wider(names_from = c("language", "keyword"),
                       values_from = "value",
                       names_glue = "{language}_{keyword}"
                       ) %>%
    dplyr::relocate(`variable-code`,
                    ends_with("variable-label"),
                    ends_with("domain"),
                    ends_with("elimination"),
                    ends_with("note")
                    ) %>%
    dplyr::full_join(px$variables1, by = "variable-code") %>%
    dplyr::relocate(names(px$variables1))

  excel_codelists <-
    px$codelists2 %>%
    dplyr::rename(`code-label` = value) %>%
    tidyr::pivot_longer(cols = -c(`variable-code`, code, language),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    tidyr::pivot_wider(names_from = c("language", "keyword"),
                       values_from = "value",
                       names_glue = "{language}_{keyword}"
                       ) %>%
    dplyr::relocate(`variable-code`,
                    code,
                    ends_with("code-label"),
                    ends_with("valuenote")
                    ) %>%
    dplyr::full_join(px$codelists1, by = c("variable-code", "code")) %>%
    dplyr::relocate(names(px$codelists1)) %>%
    dplyr::rename(sortorder = order)


  ### Make sheets in workbook
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(df, sheet_name) {
    openxlsx::addWorksheet(wb,sheet_name, gridLines = FALSE)
    options("openxlsx.maxWidth" = 40)
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = 'auto')
    openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleLight9")
  }

  add_sheet(excel_table,     "Table")
  add_sheet(excel_table2,    "Table2")
  add_sheet(excel_variables, "Variables")
  add_sheet(excel_codelists, "Codelists")

  error_if_too_many_rows_for_excel(px$data)
  add_sheet(px$data, "Data")

  # if (is.null(data_path) & create_data) {
  #   error_if_too_many_rows_for_excel(rds$data)
  #   add_sheet(rds$data, "Data")
  # } else if (identical(tools::file_ext(data_path), "rds") & create_data) {
  #   saveRDS(rds$data, data_path)
  # } else if (isFALSE(create_data)) {
  #   # pass
  # } else {
  #   unexpected_error()
  # }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

get_figures_variable_from_excel <- function(excel_path) {
  figures_variable <-
    excel_path %>%
    get_variables_sheet() %>%
    dplyr::filter(toupper(pivot) == "FIGURES") %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  error_if_not_exactly_one_figures_variable(figures_variable)
  return(figures_variable)
}

#' Get specific sheet from Excel workbook
#'
#' @param sheet String. Sheet to read.
#'
#' @returns A data frame.
get_excel_sheet <- function(sheet) {
  function(excel_path) {
    error_if_excel_sheet_does_not_exist(sheet, excel_path)
    readxl::read_xlsx(excel_path, sheet = sheet)
  }
}

get_table_sheet     <- get_excel_sheet("Table")
get_table2_sheet    <- get_excel_sheet("Table2")
get_variables_sheet <- get_excel_sheet("Variables")
get_codelists_sheet <- get_excel_sheet("Codelists")
get_data_sheet      <- get_excel_sheet("Data")

# unused -------------------------------------------------------------------

#' Get all languages in Excel metadata
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns List
get_all_languages <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_table_metadata() %>%
    dplyr::filter(keyword %in% c("LANGUAGE", "LANGUAGES")) %>%
    dplyr::pull(value) %>%
    unlist() %>%
    unique()
}

#' Get non-language dependent general table metadata
#'
#' General contains metadata relevant for the entire data set, e.g. its name,
#' subject, area, contact person, etc.
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
get_table_metadata <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_table_sheet() %>%
    dplyr::filter(!is.na(keyword)) %>%
    mutate_all_vars_to_character() %>%
    dplyr::mutate(value = ifelse(keyword == "LANGUAGES",
                                 stringr::str_replace_all(value, " ", "") %>%
                                   # remove quotes to be backwards compatible
                                   stringr::str_replace_all('"', '') %>%
                                   stringr::str_split(pattern = ','),
                                 value
                                 )
                  ) %>%
    wrap_varaible_in_list(value)
}

#' Get language dependent general table metadata
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
get_table2_metadata <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_table2_sheet() %>%
    dplyr::filter(!is.na(keyword)) %>%
    mutate_all_vars_to_character() %>%
    tidyr::pivot_longer(cols = ends_with("_value"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        ) %>%
    wrap_varaible_in_list(value)
}

#' Get metadata about variables
#'
#' Variables contains the name of the variable in each language, as well as
#' their type, notes, etc.
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
get_variables_metadata <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_variables_sheet() %>%
    tidyr::pivot_longer(cols = -c(pivot, order, `variable-code`, type),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    dplyr::mutate(`variable-label` = ifelse(is.na(`variable-label`),
                                            `variable-code`,
                                            `variable-label`
                                            )
                  )
}

#' Get codelists metadata
#'
#' Codelists contains the translation between the codes that are used in the
#' data and the labels used for each language. It also contains information
#' about sort order and numeric precision for each value.
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
get_codelists_metadata <- function(excel_metadata_path, data_df) {
  data_codes <-
    data_df %>%
    dplyr::select(-any_of(get_figures_variable(excel_metadata_path))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable-code",
                        values_to = "code"
                        ) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(language = get_all_languages(excel_metadata_path)) %>%
    dplyr::arrange_all()

  variables <-
    get_variables_metadata(excel_metadata_path) %>%
    dplyr::select(`variable-code`, language, `variable-label`)

  codelists <-
    excel_metadata_path %>%
    get_codelists_sheet() %>%
    dplyr::mutate(across(-sortorder, as.character),
                  across( sortorder, as.numeric)
                  ) %>%
    tidyr::pivot_longer(cols = ends_with(c("_code-label", "_valuenote")),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    # Add valuenote if it doesn't exist
    dplyr::bind_rows(dplyr::tibble(valuenote = character())) %>%
    dplyr::rename(value = `code-label`) %>%
    dplyr::full_join(data_codes, by = c("variable-code", "code", "language")) %>%
    dplyr::left_join(variables, by = c("variable-code", "language")) %>%
    dplyr::mutate(value = ifelse(is.na(value), code, value))

  return(codelists)
}
