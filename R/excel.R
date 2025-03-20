excel_sheet_exists <- function(sheet, excel_path) {
  sheet %in% readxl::excel_sheets(excel_path)
}

#' Get specific sheet from Excel workbook
#'
#' @param sheet String. Sheet to read.
#' @param add_automatically Logical. If TRUE, return an empty data frame if the
#' sheet does not exist.
#'
#' @returns A data frame.
#' @keywords internal
get_excel_sheet <- function(sheet, add_automatically = FALSE) {
  function(excel_path) {
    if (add_automatically) {
      if (! excel_sheet_exists(sheet, excel_path)) {
        return(data.frame())
      }
    } else {
      error_if_excel_sheet_does_not_exist(sheet, excel_path)
    }

    readxl::read_xlsx(excel_path, sheet) %>%
      drop_blank_rows()
  }
}

get_table_sheet       <- get_excel_sheet("Table")
get_table2_sheet      <- get_excel_sheet("Table2")
get_variables_sheet   <- get_excel_sheet("Variables")
get_cells_sheet       <- get_excel_sheet("Cells")
get_acrosscells_sheet <- get_excel_sheet("Acrosscells", add_automatically = TRUE)
get_data_sheet        <- get_excel_sheet("Data", add_automatically = FALSE)

#' Get figures variable from Excel workbook
#'
#' @inheritParams px_from_excel
#'
#' @returns Character, name of figures variable.
#' @keywords internal
get_figures_variable_from_excel <- function(excel_path) {
  figures_variable <-
    excel_path %>%
    get_variables_sheet() %>%
    dplyr::filter(toupper(.data$pivot) == "FIGURES") %>%
    dplyr::distinct(.data$`variable-code`) %>%
    dplyr::pull(.data$`variable-code`)

  error_if_not_exactly_one_figures_variable(figures_variable)

  return(figures_variable)
}

#' Create a px object from an Excel workbook
#'
#' @param excel_path Path to Excel metadata workbook.
#' @param data A data frame if data isen't stored in the Excel workbook.
#'
#' @returns A px object.
#' @keywords internal
px_from_excel <- function(excel_path, data = NULL) {
  # languages, table1
  table_sheet <-
    get_table_sheet(excel_path) %>%
    dplyr::filter(!is.na(.data$keyword))

  languages <-
    table_sheet %>%
    dplyr::filter(.data$keyword %in% c("LANGUAGES")) %>%
    dplyr::mutate(value =  stringr::str_replace_all(.data$value, " ", "") %>%
                    # remove quotes to be backwards compatible
                    stringr::str_replace_all('"', '') %>%
                    stringr::str_split(pattern = ',')
                  ) %>%
    tidyr::unnest("value") %>%
    tidyr::drop_na("value") %>%
    dplyr::select("language" = "value") %>%
    align_data_frames(get_base_languages())

  table1 <-
    table_sheet %>%
    align_data_frames(get_base_table1()) %>%
    dplyr::filter(! .data$keyword %in% c("LANGUAGES")) %>%
    sort_table1()

  # table2
  table2 <-
    excel_path %>%
    get_table2_sheet() %>%
    dplyr::filter(!is.na(.data$keyword)) %>%
    tidyr::pivot_longer(cols = ends_with("_value"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        ) %>%
    align_data_frames(get_base_table2()) %>%
    sort_table2(languages = languages$language)

  # variables1, variables2
  variables_sheet <-
    excel_path %>%
    get_variables_sheet()

  variables1 <-
    variables_sheet %>%
    align_data_frames(get_base_variables1()) %>%
    sort_variables1() %>%
    dplyr::select("variable-code", "pivot", "order",
                  "variable-type", "contvariable", "timeval"
                  )

  # data_df, variables2, cells1, cells2
  if (is.null(data)) {
    if (excel_sheet_exists("Data", excel_path)) {
      data <- get_data_sheet(excel_path)
    } else {
      # Empty dummy data set
      data <-
        variables_sheet %>%
        dplyr::pull("variable-code") %>%
        rlang::rep_named(list(as.character())) %>%
        dplyr::as_tibble()
    }
  }

  data_df <- format_data_df(data,
                            figures_variable = get_figures_variable_from_excel(excel_path)
                            )

  variables2 <-
    variables_sheet %>%
    dplyr::select(-all_of(intersect(c("pivot", "order", "variable-type",
                                      "contvariable", "timeval"
                                      ),
                                    names(.)
                                    )
                          )
                  ) %>%
    tidyr::pivot_longer(cols = -c("variable-code"),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    align_data_frames(get_base_variables2()) %>%
    dplyr::mutate(`variable-label` = ifelse(is.na(.data$`variable-label`),
                                            .data$`variable-code`,
                                            .data$`variable-label`
                                            )
                  ) %>%
    sort_variables2(data_table_names = names(data_df),
                    languages = languages$language
                    )

  cells_sheet <-
    excel_path %>%
    get_cells_sheet()

  cells1 <-
    cells_sheet %>%
    align_data_frames(get_base_cells1()) %>%
    dplyr::select("variable-code", "code", "order" = "sortorder", "precision")

  cells2 <-
    cells_sheet %>%
    dplyr::select(-"sortorder", -"precision") %>%
    tidyr::pivot_longer(cols = ends_with(c("_code-label", "_valuenote")),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword") %>%
    dplyr::rename("value" = "code-label") %>%
    align_data_frames(get_base_cells2())

  # acrosscells
  stub_heading_variables <-
    variables1 %>%
    dplyr::filter(toupper(.data$pivot) %in% c("STUB", "HEADING")
                  ) %>%
    dplyr::pull("variable-code")

  acrosscells <-
    excel_path %>%
    get_acrosscells_sheet() %>%
    { if (ncol(.) != 0) {
      tidyr::pivot_longer(.,
                          cols = ends_with(c("cellnote", "cellnotex")),
                          names_to = c("language", "keyword"),
                          names_pattern = "^([[:alpha:]]+)_(.*)$"
      ) %>%
        tidyr::pivot_wider(names_from = "keyword")
    } else {
      .
    }} %>%
    align_data_frames(get_base_acrosscells(stub_heading_variables))

  new_px(languages = languages,
         table1 = table1,
         table2 = table2,
         variables1 = variables1,
         variables2 = variables2,
         cells1 = cells1,
         cells2 = cells2,
         acrosscells = acrosscells,
         data = data_df
         )
}

#' Add a data frame as a sheet to an Excel workbook
#'
#' @param wb An Excel workbook
#' @param df A data frame
#' @param sheet_name Name of the sheet
#'
#' @returns Nothing
#' @keywords internal
add_excel_sheet <- function(wb, df, sheet_name) {
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
  options("openxlsx.maxWidth" = 40)
  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = 'auto')
  openxlsx::writeDataTable(wb, sheet_name, df, tableStyle = "TableStyleLight9")
}


#' Save px object as an Excel workbook
#'
#' @param x A px object
#' @param path Path to save Excel workbook
#' @inheritParams px_save
#'
#' @returns Nothing
#' @keywords internal
save_px_as_xlsx <- function(x, path, save_data, data_path) {
  excel_table <-
    data.frame(keyword ="LANGUAGES",
               value = paste0(x$languages$language, collapse = ",")
               ) %>%
    tidyr::drop_na("value") %>%
    dplyr::bind_rows(x$table1) %>%
    dplyr::arrange(.data$keyword)


  excel_table2 <-
    x$table2 %>%
    tidyr::pivot_wider(names_from = "language",
                       values_from = "value",
                       names_glue = "{language}_value"
                       )

  excel_variables <-
    x$variables2 %>%
    tidyr::pivot_longer(cols = -all_of(intersect(c("variable-code",
                                                   "language",
                                                   "contvariable",
                                                   "timeval"
                                                   ),
                                                 names(.)
                                                 )
                                       ),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    tidyr::pivot_wider(names_from = c("language", "keyword"),
                       values_from = "value",
                       names_glue = "{language}_{keyword}"
                       ) %>%
    dplyr::relocate("variable-code",
                    ends_with("variable-label"),
                    ends_with("domain"),
                    ends_with("elimination"),
                    ends_with("note")
                    ) %>%
    dplyr::full_join(x$variables1, by = "variable-code") %>%
    dplyr::relocate(names(x$variables1))

  excel_cells <-
    x$cells2 %>%
    dplyr::rename("code-label" = "value") %>%
    tidyr::pivot_longer(cols = -c("variable-code", "code", "language"),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
    tidyr::pivot_wider(names_from = c("language", "keyword"),
                       values_from = "value",
                       names_glue = "{language}_{keyword}"
                       ) %>%
    dplyr::relocate("variable-code",
                    "code",
                    ends_with("code-label"),
                    ends_with("valuenote")
                    ) %>%
    dplyr::full_join(x$cells1, by = c("variable-code", "code")) %>%
    dplyr::relocate(names(x$cells1)) %>%
    dplyr::rename("sortorder" = "order")


  empty_acrosscells <-
    dplyr::bind_cols(dplyr::tibble("language" = defined_languages(x)),
                     lapply(x$acrosscells, function(x) NA) %>%
                       dplyr::as_tibble() %>%
                       dplyr::select(-"language")
                     )

  excel_acrosscells <-
    x$acrosscells %>%
    dplyr::bind_rows(empty_acrosscells) %>%
    tidyr::pivot_longer(cols = setdiff(names(get_base_acrosscells()), "language"),
                        names_to = "keyword",
                        values_to = "value"
                        ) %>%
      tidyr::pivot_wider(names_from = c("language", "keyword"),
                         values_from = "value",
                         names_glue = "{language}_{keyword}"
                         ) %>%
      dplyr::relocate(ends_with("cellnote"),
                      ends_with("cellnotex"),
                      .after = last_col()
                      ) %>%
      drop_blank_rows()


  ### Make sheets in workbook
  wb <- openxlsx::createWorkbook()

  add_excel_sheet(wb, excel_table,      "Table")
  add_excel_sheet(wb, excel_table2,     "Table2")
  add_excel_sheet(wb, excel_variables,  "Variables")
  add_excel_sheet(wb, excel_cells,      "Cells")
  add_excel_sheet(wb, excel_acrosscells, "Acrosscells")

  if (save_data) {
    if (is.null(data_path)) {
      error_if_too_many_rows_for_excel(x$data)
      add_excel_sheet(wb, x$data, "Data")
    } else if (is_rds_file(data_path)) {
      saveRDS(x$data, data_path)
    } else if (is_parquet_file(data_path)) {
      arrow::write_parquet(x$data, data_path)
    } else {
      unexpected_error()
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
