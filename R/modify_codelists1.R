#' Handle precision and precisionX
#'
#' @rdname precision.px
#' @param type A character string, precision or precisionX
handle_codelists1 <- function(x, value, column) {
  if (missing(value)) {
    result <- get_codelists1_value(x, column)

    if (nrow(result) == 0) {
      return(NULL)
    } else {
      return(result)
    }
  } else if (is.null(value)) {
    x$codelists1[[column]] <- NA
    return(x)
  }

  validate_px(modify_codelists1(x, column, value))
}

#' @rdname precision.px
#' @export
precision <- function(x, value) {
  UseMethod("precision")
}

#' PRECISION
#'
#' Inspect or change PRECISION.
#'
#' @param x A px object
#' @param value Optional. A data frame with the column 'precision' and one or
#' more of the columns: 'variable-code' and 'code'. If value is missing, all
#' current precisions are returned. If NULL, all precisions are removed.
#'
#' @export
precision.px <- function(x, value) {
  handle_codelists1(x, value, "precision")
}


#' @rdname order.px
#' @export
order <- function(x, value) {
  UseMethod("order")
}

#' ORDER
#'
#' Inspect or change ORDER.
#'
#' @param x A px object
#' @param value Optional. A data frame with the column 'order' and one or
#' more of the columns: 'variable-code' and 'code'. If value is missing, all
#' current orders are returned. If NULL, all orders are removed.
#'
#' @export
order.px <- function(x, value) {
  handle_codelists1(x, value, "order")
}
