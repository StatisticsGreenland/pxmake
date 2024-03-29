#' Create a px object
#'
#' Create a px object from a px file, an Excel metadata workbook, or a data
#' frame.
#'
#' @param input Path to px file, path to an Excel metadata workbook, a data
#' frame or path to an .rds file with a data frame. If input is a data frame, a
#' px object with minimal metadata is created.
#' @param data Either a data frame or a path to an `.rds` file with a data frame.
#' This can only be used if `input` is an Excel metadata workbook. If NULL, the
#' data should be provided in the 'Data' sheet of the Excel workbook.
#'
#' @return A px object
#'
#' @export
px <- function(input, data = NULL) {
  validate_px_arguments(input, data)

  if (is_rds_file(input)) {
    input <- readRDS(input)
  }

  if (is_rds_file(data)) {
    data <- readRDS(data)
  }

  if (is_px_file(input)) {
    px <- px_from_px_file(input)
  } else if (is_xlsx_file(input)) {
    px <- px_from_excel(input, data)
  } else if (is.data.frame(input)) {
    px <- px_from_data_df(input)
  } else {
    unexpected_error()
  }

  validate_px(px)
}

#' Save px object to file
#'
#' @param x A px object.
#' @param path Path to file. The file extension determines the format. Can be:
#' - `.px` to save as a px file
#' - `.xlsx` to save as an Excel metadata workbook
#' @param save_data If FALSE, no 'Data' sheet is created in the Excel workbook.
#' Can only be used if `path` is an `.xlsx` file.
#' @param data_path Path to an `.rds` file to save data table at. This is usefull
#' when saving an Excel workbook where the data has more rows than Excel can
#' handle. Can only be used if 'path' is an `.xlsx` file, and 'save_data' is
#' TRUE.
#'
#' @return Nothing
#' @export
pxsave <- function(x, path, save_data = TRUE, data_path = NULL) {
  validate_pxsave_arguments(x, path, save_data, data_path)

  if (is_px_file(path)) {
    save_px_as_px_file(x, path)
  } else if (is_xlsx_file(path)) {
    save_px_as_xlsx(x, path, save_data, data_path)
  } else {
    unexpected_error()
  }
}

#' Create new px object
#'
#' px constructor for internal functions
#'
#' @param languages A data frame with language metadata.
#' @param table1 A data frame with language independent table metadata.
#' @param table2 A data frame with language dependent table metadata.
#' @param variables1 A data frame with language independent variable metadata.
#' @param variables2 A data frame with language dependent variable metadata.
#' @param codelists1 A data frame with language independent codelist metadata.
#' @param codelists2 A data frame with language dependent codelist metadata.
#' @param data A data frame with data.
#'
#' @return A px object
new_px <- function(languages, table1, table2, variables1, variables2,
                   codelists1, codelists2, data) {
  x <- list(languages  = dplyr::as_tibble(languages),
            table1     = dplyr::as_tibble(table1),
            table2     = dplyr::as_tibble(table2),
            variables1 = dplyr::as_tibble(variables1),
            variables2 = dplyr::as_tibble(variables2),
            codelists1 = dplyr::as_tibble(codelists1),
            codelists2 = dplyr::as_tibble(codelists2),
            data       = dplyr::as_tibble(data)
            )

  structure(x, class = "px")
}

#' Fix some common issues in px objects
#'
#' @param x A px object.
#'
#' @return A px object
fix_px <- function(x) {
  undefined_variables <- setdiff(colnames(x$data), x$variables2$`variable-code`)

  if (length(undefined_variables) > 0) {
    x$variables2 <-
      dplyr::bind_rows(x$variables2,
                       dplyr::tibble(`variable-code` = undefined_variables)
                       )
  }

  # Add missing variable-labels to variables2
  x$variables2 <-
    x$variables2 %>%
    dplyr::mutate(`variable-label` = ifelse(is.na(`variable-label`),
                                            `variable-code`,
                                            `variable-label`)
                  )

  x
}

#' Validate px object
#'
#' Throws an error if the px object is not valid.
#'
#' @param x A supposed px object.
#'
#' @return A valid px object.
validate_px <- function(x) {
  error_if_not_list(x)
  error_if_not_class_px(x)
  error_if_not_list_of_data_frames(x)
  error_if_list_names_are_wrong(x)
  error_if_data_frame_is_missing_column(x)
  error_if_multiple_time_variables(x)
  error_if_variable_label_is_na(x)
  error_if_misplaced_keywords_in_table(x, table_name = "table1")
  error_if_misplaced_keywords_in_table(x, table_name = "table2")
  error_if_variable_code_not_in_data(x, "variables1")
  error_if_variable_code_not_in_data(x, "variables2")
  error_if_variable_code_not_in_data(x, "codelists1")
  error_if_variable_code_not_in_data(x, "codelists2")
  error_if_data_column_is_not_defined(x, "variables1")
  error_if_data_column_is_not_defined(x, "variables2")
  error_if_used_languages_are_not_defined(x)
  error_if_language_not_in_languages(x)

  return(x)
}
