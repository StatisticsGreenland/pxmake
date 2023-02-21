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
