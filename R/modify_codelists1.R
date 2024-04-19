#' Handle codelists changes
#'
#' @rdname precision.px
#' @param number The number of the codelist (1 or 2)
#' @param column The name of the codelists column
handle_codelists <- function(x, value, number, column) {
  if (missing(value)) {
    result <- get_codelists_value(x, number, column)

    if (nrow(result) == 0) {
      return(NULL)
    } else {
      return(result)
    }
  } else if (is.null(value)) {
    x[[get_codelists_name(number)]][[column]] <- NA
    return(x)
  }

  validate_px(modify_codelists(x, number, column, value))
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
  handle_codelists(x, value, "1", "precision")
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
  handle_codelists(x, value, "1", "order")
}
