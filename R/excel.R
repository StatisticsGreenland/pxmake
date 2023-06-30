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
get_data_sheet      <- get_excel_sheet("Data")

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
#' General contains metadata relevant for the entire data set, e.g. its name,
#' subject, area, contact person, etc.
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
    tidyr::pivot_longer(cols = -c(pivot, order, `variable-code`, type),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
    ) %>%
    tidyr::pivot_wider(names_from = "keyword")
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
    tidyr::pivot_longer(cols = ends_with("_code-label"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        ) %>%
    dplyr::full_join(data_codes, by = c("variable-code", "code", "language")) %>%
    dplyr::left_join(variables, by = c("variable-code", "language")) %>%
    dplyr::mutate(value = ifelse(is.na(value), code, value))

  return(codelists)
}
