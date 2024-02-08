#' Create a pxobject from a px-file
#'
#' This function will be deprecated in the future. Use `px` and `pxsave` instead.
#'
#' @param input Input can be provided in one of three ways:
#' 1. A path to a `.px` file.
#' 1. A path to a `.rds` file created by \link{pxmake}.
#' @param out_path Path to save metadata at. Use `.xlsx` extension to save
#' as an Excel workbook. Use `.rds` to save as an rds file. If NULL, no file is
#' saved.
#' @param data_path Path to save data as an .rds file. If NULL, the data is
#' saved as the sheet 'Data' in the Excel metadata workbook.
#' @param create_data Logic. If FALSE the data table is not generated. This can
#' be used to only generate metadata.
#'
#' @returns Returns rds object invisibly.
#'
#' @seealso \link{pxmake}
#'
#' @export
metamake <- function(input, out_path = NULL, data_path = NULL, create_data = TRUE) {
  validate_metamake_arguments(input, out_path, data_path, create_data)

  if (is_px_file(input)) {
    p <- px(input)
  } else if (is.data.frame(input)) {
    # pxtemplate
  } else {
    unexpected_error()
  }

  if (is_xlsx_file(out_path)) {
    pxsave(p, out_path)
  }

  invisible(p)
}
