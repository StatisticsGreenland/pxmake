# Helper functions for throwing errors, warnings and notes.
error_if_excel_sheet_does_not_exist <- function(sheet_name, excel_path) {
  if (! sheet_name %in% readxl::excel_sheets(excel_path)) {
    stop(stringr::str_glue("The sheet '{sheet_name}' is missing in: ",
                           "{excel_path}.\nAdd the sheet or use the argument ",
                           "'data_path ='."
                           )
         )
  }
}

error_if_more_than_one_time_variable <- function(time_variable) {
  if (length(time_variable) > 1) {
    stop(stringr::str_glue("The metadata has more than one variable where ",
                           "`type=time`. Change the type of the variables ",
                           "{time_variable}, so only one has `type=time`."
                           )
         )
  }
}

error_if_not_exactly_one_figures_variable <- function(figures_var) {
  if (length(figures_var) == 0) {
    stop(stringr::str_glue("There is no figures variable. Change the metadata ",
                           "so one variable has `type=figures`."
                           )
         )
  } else if (length(figures_var) > 1) {
    stop(stringr::str_glue("There are more than one variable with figures. ",
                           "Change the type of the variables {figures_var}, so ",
                           "only one has `type=figures`."
                           )
         )
  }
}

error_if_not_exactly_one_data_line <- function(data_line_index) {
  if(length(data_line_index) != 1) {
    stop(stringr::str_glue("There are {length(data_line_index)} lines in the ",
                           "px-file like this: 'DATA='. There needs to be ",
                           "exactly 1."
                           )
         )
  }
}

error_if_too_many_rows_for_excel <- function(df) {
  data_lines <- nrow(df)
  excel_max_lines <- 1048576

  if(data_lines > excel_max_lines) {
   stop(stringr::str_glue("The data cube contains {data_lines} data lines ",
                          "which is more than the {excel_max_lines} lines ",
                          "supported by Excel. Use argument `rds_data_path=` ",
                          "to store the data in an .rds file instead."
                          )
        )
  }
}

unexpected_error <- function() {
  stop(paste("An unexpected error occurred. Please report the issue on GitHub,",
             "ideally with a minimal reproducible example.",
             "https://github.com/StatisticsGreenland/pxmake/issues "
             )
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
  if (!is_rds_file(out_path) & !is_px_file(out_path) & !is.null(out_path)) {
    stop("Argument 'out_path' should be a path to an .rds or .px file or NULL.")
  }

  if (!is.null(add_totals)) {
    if (class(add_totals) != "character") {
      stop("Argument 'add_totals' should be of type 'character'.")
    }

    if (!is_xlsx_file(input)) {
      stop("Argument 'add_totals' can only be used when 'input' is an .xlsx file.")
    }
  }

  if (!is_xlsx_file(input) &
      !is_rds_file(input) &
      !is.data.frame(input) &
      !is_rds_list(input)
      ) {
    stop("Argument 'input' has wrong format. See ?pxmake.")
  }

  if (is.null(data)) {
    if (is.data.frame(input)) {
      stop("No data is provided. See ?pxmake.")
    } else if (is_xlsx_file(input)) {
      error_if_excel_sheet_does_not_exist("Data", input)
    }
  } else if (!is.data.frame(data) & !is_rds_file(data)) {
    stop("Argument 'data' must be a data frame or a path to an .rds file.")
  } else if (!is_xlsx_file(input) & !is.data.frame(input)) {
    stop("Argument 'data' can only be used if 'input' is an .xlsx file or a data frame.")
  }
}

#' Check all metamake arguments
#'
#' @inherit validate_pxmake_arguments description
#'
#' @inheritParams metamake
#'
#' @returns Nothing
validate_metamake_arguments <- function(input, out_path, data_path) {
  if (!is_px_file(input) & !is_rds_file(input) & !is_rds_list(input)) {
    stop("Argument 'input' has wrong format. See ?metamake.")
  }

  if (!is_xlsx_file(out_path) & !is_rds_file(out_path) & !is.null(out_path)) {
    stop("Argument 'output' needs to be an .xlsx or .rds file or NULL.")
  }

  if (!is.null(data_path)) {
    if (!is_rds_file(data_path)) {
      stop("Argument 'data_path' should be an .rds file.")
    }

    if (!is_xlsx_file(out_path)) {
      stop("Argument 'data_path' can only be used when 'input' is an .xlsx file.")
    }
  }
}
