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

#' @title PRECISION
#'
#' @description `r table_description("PRECISION")`
#'
#' @param x A px object
#' @param value `r codelists_param_value("PRECISION", "1")`
#'
#' @return A px object or a data frame
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

#' @inherit precision.px
#' @title ORDER
#' @description `r table_description("ORDER")`
#' @param value `r codelists_param_value("ORDER", "1")`
#' @export
order.px <- function(x, value) {
  handle_codelists(x, value, "1", "order")
}
