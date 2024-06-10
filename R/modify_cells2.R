#' @rdname px_valuenote.px
#' @export
px_valuenote <- function(x, value) {
  UseMethod("px_valuenote")
}

#' @inherit px_precision.px
#' @title VALUENOTE
#' @description `r table_description("VALUENOTE")`
#' @param value `r cells_param_value("VALUENOTE", "2")`
#' @export
px_valuenote.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenote")
}


#' @rdname px_valuenotex.px
#' @export
px_valuenotex <- function(x, value) {
  UseMethod("px_valuenotex")
}

#' @inherit px_precision.px
#' @title VALUENOTEX
#' @description `r table_description("VALUENOTEX")`
#' @param value `r cells_param_value("VALUENOTEX", "2")`
#' @export
px_valuenotex.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenotex")
}


#' @rdname px_values.px
#' @export
px_values <- function(x, value) {
  UseMethod("px_values")
}

#' @inherit px_precision.px
#' @title VALUES
#' @description `r table_description("VALUES")`
#' @param value `r cells_param_value("VALUES", "2")`
#' @export
px_values.px <- function(x, value) {
  handle_cells(x, value, "2", "value")
}
