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
get_variables_sheet <- get_excel_sheet("Variables")
get_codelists_sheet <- get_excel_sheet("Codelists")

#' Find 'Data table' sheet in Excel workbook
#'
#' Returns the index of the 'Data table' sheet. To be backwards compatible any
#' sheet that starts with 'Data' is accepted.
#'
#' @inheritParams get_data_table_sheet
#'
#' @returns Numeric
get_data_sheet_index <- function(excel_path) {
  sheets <- readxl::excel_sheets(excel_path)

  sheet_index <- stringr::str_which(sheets, "^Data.*")[1]

  if (!is.numeric(sheet_index) | is.na(sheet_index) | length(sheet_index) != 1) {
    error_if_excel_sheet_does_not_exist("Data table", excel_path)
  }

  return(sheet_index)
}

#' Get 'Data table' sheet from Excel workbook
#'
#' To be backwards compatible the function returns any sheet starting with
#' 'Data'.
#'
#' @param excel_path Path to an xlsx workbook
#'
#' @returns A data frame.
get_data_table_sheet <- function(excel_path) {
  readxl::read_xlsx(excel_path, sheet = get_data_sheet_index(excel_path))
}

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

#' Get general table metadata
#'
#' General contains metadata about the entire data table, e.g. its name, subject
#' area, contact person, etc.
#'
#' @inheritParams get_metadata_df_from_excel
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
#' Variables contains the name of the variable in each language, as well as
#' their type, notes, etc.
#'
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
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
#' @inheritParams get_metadata_df_from_excel
#'
#' @returns A data frame
get_codelists_metadata <- function(excel_metadata_path, data_table_df) {
  data_table_codes <-
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
    dplyr::full_join(data_table_codes, by = c("variable", "code", "language")) %>%
    # Add long_name (VARIABLECODE) (change when implementing #139)
    dplyr::left_join(variables %>% dplyr::select(variable, language, long_name),
                     by = c("variable", "language")
                     ) %>%
    dplyr::mutate(value = ifelse(is.na(value), code, value))

  return(codelists)
}
