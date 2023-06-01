#' Get specific sheet from Excel workbook
#'
#' @param sheet String. Sheet to read.
#'
#' @inherit get_table_sheet
get_excel_sheet <- function(path, sheet) {
  error_if_excel_sheet_does_not_exist(sheet, path)
  readxl::read_xlsx(path, sheet = sheet)
}

#' Get sheet 'Table' from Excel workbook
#'
#' @param path Path to Excel workbook (xls/xlsx).
#'
#' @returns A data frame
get_table_sheet <- function(path) {
  get_excel_sheet(path, "Table")
}

#' Get sheet 'Variables' from Excel workbook
#'
#' @inherit get_table_sheet
get_variables_sheet <- function(path) {
  get_excel_sheet(path, "Variables")
}

#' Get sheet 'Codelists' from Excel workbook
#'
#' @inherit get_table_sheet
get_codelists_sheet <- function(path) {
  get_excel_sheet(path, "Codelists")
}

#' Get sheet 'Data' from Excel workbook
#'
#' @inherit get_table_sheet
get_data_sheet <- function(path) {
  get_excel_sheet(path, "Data")
}

#' Get the name of figures variable
#'
#' An error is thrown if there is not exactly one figures variable.
#'
#' @param excel_metadata_path Path to Excel workbook with metadata.
#'
#' @returns Character
get_figures_variable <- function(excel_metadata_path) {
  figures_var <-
    excel_metadata_path %>%
    get_variables_sheet() %>%
    dplyr::filter(toupper(type) == "FIGURES") %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)

  error_if_not_exactly_one_figures_variable(figures_var)

  return(figures_var)
}

#' Get all languages in metadata
#'
#' @inheritParams get_figures_variable
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

#' Get general table metadata
#'
#' General contains metadata about the entire data table, e.g. its name, subject
#' area, contact person, etc.
#'
#' @inheritParams get_figures_variable
#'
#' @returns A data frame
get_table_metadata <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_table_sheet() %>%
    tidyr::separate(keyword,
                    c("keyword", "language"),
                    sep = "_(?=[[:alpha:]]+)",
                    fill = "right"
                    ) %>%
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

#' Get metadata about variables
#'
#' Varaibles contains the name the variable in each language, as well as their
#' type, notes, etc.
#'
#' @inherit get_table_metadata
get_variables_metadata <- function(excel_metadata_path) {
  excel_metadata_path %>%
    get_variables_sheet() %>%
    tidyr::pivot_longer(cols = -c(position, variable, type),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
    ) %>%
    tidyr::pivot_wider(names_from = "keyword")
}

#' Get codelists metadata
#'
#' Codelists contains the translation between the codes that are used in the
#' data table and the labels used for each language. It also contains
#' information about sort order and numeric precision for each value.
#'
#' @inherit get_table_metadata
#' @param data_table_df A data frame with data
get_codelists_metadata <- function(excel_metadata_path, data_table_df) {
  source_data_codes <-
    data_table_df %>%
    dplyr::select(-any_of(get_figures_variable(excel_metadata_path))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable",
                        values_to = "code"
                        ) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(language = get_all_languages(excel_metadata_path)) %>%
    dplyr::arrange_all()

  variables <-
    get_variables_metadata(excel_metadata_path) %>%
    dplyr::select(variable, language, long_name)

  codelists <-
    excel_metadata_path %>%
    get_codelists_sheet() %>%
    dplyr::mutate(across(-sortorder, as.character),
                  across( sortorder, as.numeric)
                  ) %>%
    tidyr::pivot_longer(cols = ends_with("_code_label"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        ) %>%
    dplyr::full_join(source_data_codes, by = c("variable", "code", "language")) %>%
    # Add long_name (VARIABLECODE) (change when implementing #139)
    dplyr::left_join(variables %>% dplyr::select(variable, language, long_name),
                     by = c("variable", "language")
                     ) %>%
    dplyr::mutate(value = ifelse(is.na(value), code, value))

  return(codelists)
}
