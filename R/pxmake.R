#' Create a px-file from metadata and data
#'
#' This function will be deprecated in the future. Use `px` and `pxsave` instead.
#'
#' @param input Path to a `.xlsx` metadata file.
#' @param out_path Path to save output at, either as an `.rds`, `.px` or `.xlsx`
#' file. If NULL, no file is saved.
#' @param data Either a data frame, or a path to an `.rds` file. If NULL,
#' the data must be provided as part of the `metadata` argument, either in
#' option 1. as the sheet 'Data' in the Excel metadata workbook, or as the
#' "data" in option 2.
#' @param add_totals A list of variables to add a 'total' level to. The option
#' is only available if `input` is an `.xlsx` file (option 1). The value of the
#' total level is looked up in 'Variables' xx_elimination. The code for the
#' level is found in 'Codelists'. The total is a sum of the values in the
#' variables with `pivot = FIGURES` in 'Variables'. NAs are ignored when summing.
#'
#' @return Returns px object invisibly.
#'
#' @seealso \link{metamake}
#'
#' @export
pxmake <- function(input,
                   out_path = NULL,
                   data = NULL,
                   add_totals = NULL) {

  validate_pxmake_arguments(input, out_path, data, add_totals)

  x <- px(input, data)

  if (! is.null(add_totals)) {
    x <- add_totals(x, variables = add_totals)
  }

  if (! is.null(out_path)) {
    pxsave(x, out_path)
  }

  invisible(x)
}
