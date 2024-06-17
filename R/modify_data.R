#' @rdname px_data.px
#' @export
px_data <- function(x, value) {
  UseMethod("px_data")
}

#' @title DATA
#'
#' @description `r table_description("DATA")`
#'
#' @param x A px object
#'
#' @param value A data frame
#'
#' @return A px object or a data frame
#'
#' @export
px_data.px <- function(x, value) {
  if (missing(value)) {
    return(x$data)
  } else {
    x$data <- value
  }

  return(validate_px(x))
}
