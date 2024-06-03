#' @rdname valuenote.px
#' @export
valuenote <- function(x, value) {
  UseMethod("valuenote")
}

#' @inherit precision.px
#' @title VALUENOTE
#' @description `r table_description("VALUENOTE")`
#' @param value `r cells_param_value("VALUENOTE", "2")`
#' @export
valuenote.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenote")
}


#' @rdname valuenotex.px
#' @export
valuenotex <- function(x, value) {
  UseMethod("valuenotex")
}

#' @inherit precision.px
#' @title VALUENOTEX
#' @description `r table_description("VALUENOTEX")`
#' @param value `r cells_param_value("VALUENOTEX", "2")`
#' @export
valuenotex.px <- function(x, value) {
  handle_cells(x, value, "2", "valuenotex")
}
