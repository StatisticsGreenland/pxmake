handle_cells <- function(x, value, number, column, validate) {
  if (missing(value)) {
    result <- get_cells_value(x, number, column)

    if (nrow(result) == 0) {
      return(NULL)
    } else {
      return(result)
    }
  } else if (is.null(value)) {
    x[[get_cells_name(number)]][[column]] <- NA
    return(x)
  }

  return_px(modify_cells(x, number, column, value), validate)
}

#' @rdname px_precision.px
#' @export
px_precision <- function(x, value, validate) {
  UseMethod("px_precision")
}

#' @eval add_doc_keyword_function_intro("PRECISION")
#' @param value `r cells_param_value("PRECISION", "1")`
#' @eval param_validate()
#' @eval add_return_px_or_df()
#' @eval add_cells1_example("PRECISION", 2, 3)
#' @export
px_precision.px <- function(x, value, validate= TRUE) {
  handle_cells(x, value, "1", "precision", validate)
}


#' @rdname px_order.px
#' @export
px_order <- function(x, value, validate) {
  UseMethod("px_order")
}

#' @title Change value order
#' @eval add_doc_keyword_function_intro("ORDER")
#' @param value `r cells_param_value("ORDER", "1")`
#' @eval param_validate()
#' @eval add_return_px_or_df()
#' @eval add_cells1_example("ORDER", 8, 9)
#' @export
px_order.px <- function(x, value, validate = TRUE) {
  handle_cells(x, value, "1", "order", validate)
}
