#' Create a px object
#'
#' Create a px object from a px file, an Excel metadata workbook, or a data
#' frame.
#'
#' @param input Path to px file, path to an Excel metadata workbook, a data
#' frame or path to an `.rds` or `.parquet` file with a data frame. If input is
#' a data frame or NULL, a px object with minimal metadata is created.
#' @param data Either a data frame or a path to an `.rds` or `.parquet` file
#' with a data frame. This can only be used if `input` is an Excel metadata
#' workbook.
#'
#' @return A px object
#'
#' @examples
#' # Create px object from dataset
#' x1 <- px(population_gl)
#'
#' # Create px object from px file
#' px_path <- tempfile(fileext = ".px")
#' url <- "https://bank.stat.gl:443/sq/0cf06962-19f1-4d5c-8d43-b7ed0009617d"
#' download.file(url, px_path)
#'
#' x2 <- px(px_path)
#'
#' @export
px <- function(input = NULL, data = NULL) {
  validate_px_arguments(input, data)

  if (is_rds_file(input)) {
    input <- readRDS(input)
  }

  if (is_parquet_file(input)) {
    input <- arrow::read_parquet(input)
  }

  if (is.null(input)) {
    input <- data.frame()
  }

  if (is_rds_file(data)) {
    data <- readRDS(data)
  }

  if (is_parquet_file(data)) {
    data <- arrow::read_parquet(data)
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
#' @param data_path Path to an `.rds` or `.parquet` file to save data table at.
#' This is usefull when saving an Excel workbook where the data has more rows
#' than Excel can handle. Can only be used if 'path' is an `.xlsx` file, and
#' 'save_data' is TRUE.
#' @details
#' Use `px_codepage()` to change file encoding.
#'
#' @seealso [px_codepage()]
#' @return Nothing
#' @export
px_save <- function(x, path, save_data = TRUE, data_path = NULL) {
  validate_px_save_arguments(x, path, save_data, data_path)

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
#' @param cells1 A data frame with language independent cells metadata.
#' @param cells2 A data frame with language dependent cells metadata.
#' @param acrosscells A data frame with metadata that spans multiple cells.
#' @param data A data frame with data.
#'
#' @return A px object
#' @keywords internal
new_px <- function(languages, table1, table2, variables1, variables2,
                   cells1, cells2, acrosscells, data) {
  x <- list(languages  = dplyr::as_tibble(languages),
            table1     = dplyr::as_tibble(table1),
            table2     = dplyr::as_tibble(table2),
            variables1 = dplyr::as_tibble(variables1),
            variables2 = dplyr::as_tibble(variables2),
            cells1 = dplyr::as_tibble(cells1),
            cells2 = dplyr::as_tibble(cells2),
            acrosscells = dplyr::as_tibble(acrosscells),
            data       = dplyr::as_tibble(data)
            )

  structure(x, class = "px")
}

#' Fix some common issues in px objects
#'
#' @param x A px object.
#'
#' @return A px object
#' @keywords internal
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
#' @keywords internal
validate_px <- function(x) {
  error_if_not_list(x)
  error_if_not_class_px(x)
  error_if_not_list_of_data_frames(x)
  error_if_list_names_are_wrong(x)
  error_if_data_frame_is_missing_column(x)
  error_if_multiple_time_variables(x)
  error_if_timeval_isnot_heading_or_stub(x)
  error_if_contvariable_isnot_heading_or_stub(x)
  error_if_variable_label_is_na(x)
  error_if_misplaced_keywords_in_table(x, table_name = "table1")
  error_if_misplaced_keywords_in_table(x, table_name = "table2")
  error_if_variable_code_not_in_data(x, "variables1")
  error_if_variable_code_not_in_data(x, "variables2")
  error_if_variable_code_not_in_data(x, "cells1")
  error_if_variable_code_not_in_data(x, "cells2")
  error_if_data_column_is_not_defined(x, "variables1")
  error_if_data_column_is_not_defined(x, "variables2")
  error_if_used_languages_are_not_defined(x)
  error_if_language_not_in_languages(x)
  error_if_value_contains_quotation_marks(x)

  return(x)
}
