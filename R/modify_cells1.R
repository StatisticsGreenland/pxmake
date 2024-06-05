handle_cells <- function(x, value, number, column) {
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

  validate_px(modify_cells(x, number, column, value))
}

#' @rdname px_precision.px
#' @export
px_precision <- function(x, value) {
  UseMethod("px_precision")
}

#' @title PRECISION
#'
#' @description `r table_description("PRECISION")`
#'
#' @param x A px object
#' @param value `r cells_param_value("PRECISION", "1")`
#'
#' @return A px object or a data frame
#'
#' @export
px_precision.px <- function(x, value) {
  handle_cells(x, value, "1", "precision")
}


#' @rdname px_order.px
#' @export
px_order <- function(x, value) {
  UseMethod("px_order")
}

#' @inherit px_precision.px
#' @title ORDER
#' @description `r table_description("PX_ORDER")`
#' @param value `r cells_param_value("PX_ORDER", "1")`
#' @export
px_order.px <- function(x, value) {
  handle_cells(x, value, "1", "order")
}
