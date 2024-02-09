# Helper functions for throwing errors, warnings and notes.

#' Throw error
#'
#' @param msg String, error message
#'
#' @returns Nothing
error <- function(msg) {
  stop(msg, call. = FALSE)
}

error_if_excel_sheet_does_not_exist <- function(sheet_name, excel_path) {
  if (! sheet_name %in% readxl::excel_sheets(excel_path)) {
    error(stringr::str_glue("The sheet '{sheet_name}' is missing in: {excel_path}."))
  }
}

error_if_more_than_one_time_variable <- function(time_variable) {
  if (length(time_variable) > 1) {
    error(stringr::str_glue("The metadata has more than one variable where ",
                            "`type=time`. Change the type of the variables ",
                            "{time_variable}, so only one has `type=time`."
                            )
          )
  }
}

error_if_not_exactly_one_figures_variable <- function(figures_var) {
  if (length(figures_var) == 0) {
    error(stringr::str_glue("There is no figures variable. Change the metadata ",
                            "so one variable has `pivot=FIGURES`."
                            )
         )
  } else if (length(figures_var) > 1) {
    error(stringr::str_glue("There are more than one variable with figures. ",
                            "Change the type of the variables {figures_var}, so ",
                            "only one has `pivot=FIGURES`."
                            )
         )
  }
}

error_if_not_exactly_one_data_line <- function(data_line_index) {
  if(length(data_line_index) != 1) {
    error(stringr::str_glue("There are {length(data_line_index)} lines in the ",
                            "px file like this: 'DATA='. There needs to be ",
                            "exactly 1."
                            )
         )
  }
}

error_if_too_many_rows_for_excel <- function(df) {
  data_lines <- nrow(df)
  excel_max_lines <- 1048576

  if(data_lines > excel_max_lines) {
   error(stringr::str_glue("The data cube contains {data_lines} data lines ",
                           "which is more than the {excel_max_lines} lines ",
                           "supported by Excel. Use argument `rds_data_path=` ",
                           "to store the data in an .rds file instead."
                           )
        )
  }
}

unexpected_error <- function() {
  error(paste("An unexpected error occurred. Please report the issue on GitHub,",
              "ideally with a minimal reproducible example.",
              "https://github.com/StatisticsGreenland/pxmake/issues "
              )
       )
}

#' A list of which variables should be in each sheet
get_mandatory_variables <- function() {
  list("Table"     = c("keyword", "value"),
       "Table2"    = c("keyword", "code"),
       "Variables" = c("pivot", "order", "variable-code", "variable-label", "type"),
       "Codelists" = c("variable-code", "sortorder", "code", "code-label", "precision")
  )
}

get_legal_values <- function() {
  dplyr::tribble(~sheet, ~variable, ~value,
                 "Variables", "pivot", c("FIGURES", "STUB", "HEADING"),
                 "Variables", "type",  c("TIME", "CONTVARIABLE", NA)
                 )
}

error_if_variable_has_illegal_values <- function(excel_path, sheet) {
  legal_values <-
    get_legal_values() %>%
    dplyr::filter(sheet == sheet) %>%
    dplyr::select(-sheet) %>%
    tidyr::unnest(value)

  data_values <-
    get_excel_sheet(sheet)(excel_path) %>%
    dplyr::select(dplyr::pull(legal_values, variable)) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    dplyr::mutate(value = toupper(value)) %>%
    dplyr::distinct_all()

  illegal_values <- dplyr::anti_join(data_values, legal_values,
                                     by = c("variable", "value")
                                     )

  if (nrow(illegal_values) > 0) {
    illegal_value <-
      illegal_values %>%
      head(1) %>%
      as.list()

    legal_values_list <-
      legal_values %>%
      dplyr::filter(variable == illegal_value$variable) %>%
      dplyr::pull(value) %>%
      paste(collapse="', '")

    error(stringr::str_glue(
      "The value '{illegal_value$value}' is not allowed in the variable ",
      "'{illegal_value$variable}' in the sheet '{sheet}'.\n",
      "Change it to one of the legal values: '{legal_values_list}'"
      ))
  }
}

error_if_sheet_is_missing_variable <- function(excel_path, sheet) {
  data_variable_names <-
    get_excel_sheet(sheet)(excel_path) %>%
    names() %>%
    stringr::str_split_fixed("_", n = 2) %>%
    as.data.frame() %>%
    dplyr::mutate(variable_name_without_language_code = ifelse(V2=="", V1, V2)) %>%
    dplyr::pull(variable_name_without_language_code)

  missing_variables <- setdiff(get_mandatory_variables()[sheet] %>% unlist(),
                               data_variable_names
                               )

  if (length(missing_variables) > 0) {
    error(stringr::str_glue("The sheet '{sheet}' is missing the mandatory variable ",
                            "'{missing_variables[1]}'. Please add it to the sheet."
                            )
         )
  }
}

#' Validate Excel metadata workbook
#'
#' @param excel_path Path to the Excel metadata workbook
#'
#' @returns Nothing
validate_xlsx_metadata <- function(excel_path) {
  sheets <- c("Table", "Table2", "Variables", "Codelists")

  invisible(lapply(sheets,
                   error_if_sheet_is_missing_variable,
                   excel_path = excel_path
                   )
            )
  error_if_variable_has_illegal_values(sheet = "Variables",
                                       excel_path
                                       )
}

#' Check all pxmake arguments
#'
#' Throw an error if there are problems with the function arguments.
#'
#' @inheritParams pxmake
#'
#' @returns Nothing
validate_pxmake_arguments <- function(input, out_path, data, add_totals) {
  if (!is_xlsx_file(out_path) & !is_px_file(out_path) & !is.null(out_path)) {
    error("Argument 'out_path' should be a path to an .xlsx, .px file or NULL.")
  }

  if (!is.null(add_totals)) {
    if (class(add_totals) != "character") {
      error("Argument 'add_totals' should be of type 'character'.")
    }

    if (!is_xlsx_file(input)) {
      error("Argument 'add_totals' can only be used when 'input' is an .xlsx file.")
    }
  }

  if (!is_xlsx_file(input) &
      !is_rds_file(input) &
      !is.data.frame(input)
      ) {
    error("Argument 'input' has wrong format. See ?pxmake.")
  }

  if (is_xlsx_file(input)) {
    validate_xlsx_metadata(input)
  }

  if (is.null(data)) {
    if (is.data.frame(input)) {
      error("No data is provided. See ?pxmake.")
    } else if (is_xlsx_file(input)) {
      error_if_excel_sheet_does_not_exist("Data", input)
    }
  } else if (!is.data.frame(data) & !is_rds_file(data)) {
    error("Argument 'data' must be a data frame or a path to an .rds file.")
  } else if (!is_xlsx_file(input) & !is.data.frame(input)) {
    error("Argument 'data' can only be used if 'input' is an .xlsx file or a data frame.")
  }
}

#' Check all metamake arguments
#'
#' @inherit validate_pxmake_arguments description
#'
#' @inheritParams metamake
#'
#' @returns Nothing
validate_metamake_arguments <- function(input, out_path, data_path, create_data) {
  if (!is_px_file(input) & !is_rds_file(input) & !is.data.frame(input)
      ) {
    error("Argument 'input' has wrong format. See ?metamake.")
  }

  if (!is_xlsx_file(out_path) & !is_rds_file(out_path) & !is.null(out_path)) {
    error("Argument 'out_path' needs to be an .xlsx or .rds file or NULL.")
  }

  if (!is.null(data_path)) {
    if (!is_rds_file(data_path)) {
      error("Argument 'data_path' should be an .rds file.")
    }

    if (!is_xlsx_file(out_path)) {
      error("Argument 'data_path' can only be used when 'input' is an .xlsx file.")
    }
  }

  if (!isTRUE(create_data) & !isFALSE(create_data)) {
    error("Argument 'create_data' should be TRUE or FALSE.")
  }
}

#' Check all arguments to px()
#'
#' @inheritParams px
#'
#' @return Nothing
validate_px_arguments <- function(input, data) {
  if (! any(is_px_file(input), is_xlsx_file(input), is.data.frame(input))) {
    error("Argument 'input' has wrong format. See ?px.")
  }

  if (! any(is.null(data), is.data.frame(data), is_rds_file(data))) {
    error("Argument 'data' has wrong format. See ?px.")
  }

  if (is.null(data) & is_xlsx_file(input)) {
      error_if_excel_sheet_does_not_exist("Data", input)
  }

  if (! is.null(data) & ! is_xlsx_file(input)) {
      error("Argument 'data' can only be used if 'input' is an .xlsx file.")
  }
}

#' Check all arguments to pxsave()
#'
#' @inheritParams pxsave
#'
#' @return Nothing
validate_pxsave_arguments <- function(x, path) {
  validate_px(x)

  if (! any(is_px_file(path), is_xlsx_file(path))) {
    error("Argument 'path' must be a path to a .r .xlsx file.")
  }
}

#' Check all arguments to micromake()
#'
#' @inheritParams micromake
#'
#' @return Nothing
validate_micromake_arguments <- function(x, out_dir) {
  validate_px(x)

  if (! any(is.character(out_dir), dir.exists(out_dir), is.null(out_dir))) {
    error("Argument 'out_dir' must be a character string to an existing directory.")
  }
}

