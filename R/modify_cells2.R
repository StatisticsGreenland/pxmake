#' @rdname px_valuenote.px
#' @export
px_valuenote <- function(x, value) {
  UseMethod("px_valuenote")
}

#' @eval add_doc_keyword_function_intro("VALUENOTE")
#' @param value `r cells_param_value("VALUENOTE", "2")`
#' @eval add_return_px_or_df()
#' @eval add_cells2_example("VALUENOTE", 'Counts are approximated', 'Some of the figures are from 2003', 'Kisitsisit ilaat 2003-imeersuupput')
#' @export
px_valuenote.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenote")
}


#' @rdname px_valuenotex.px
#' @export
px_valuenotex <- function(x, value) {
  UseMethod("px_valuenotex")
}

#' @eval add_doc_keyword_function_intro("VALUENOTEX")
#' @param value `r cells_param_value("VALUENOTEX", "2")`
#' @eval add_return_px_or_df()
#' @eval add_cells2_example("VALUENOTEX", 'Counts are approximated', 'Some of the figures are from 2003', 'Kisitsisit ilaat 2003-imeersuupput')
#' @export
px_valuenotex.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenotex")
}


#' @rdname px_values.px
#' @export
px_values <- function(x, value) {
  UseMethod("px_values")
}

#' @eval add_doc_keyword_function_intro("VALUES")
#' @param value `r cells_param_value("VALUES", "2")`
#' @eval add_return_px_or_df()
#' @eval add_cells2_example("VALUES", 'Year 2024', 'toddler', 'meeraaqqap')
#' @export
px_values.px <- function(x, value) {
  handle_cells(x, value, "2", "value")
}
