# Helper functions for throwing errors, warnings and notes.

error_if_excel_sheet_does_not_exist <- function(sheet_name, excel_path) {
  if (! sheet_name %in% readxl::excel_sheets(excel_path)) {
    stop(stringr::str_glue("The sheet {sheet_name} is missing in: ",
                           "{excel_path}. Add the sheet or provide a ",
                           "source_data_path."
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
