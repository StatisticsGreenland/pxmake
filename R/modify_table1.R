modify_table1 <- function(x, keyword, value) {
  x$table1 <- modify_or_add_row(x$table1, "keyword", keyword, "value", value)
  return(x)
}

#' @rdname charset.px
#' @export
charset <- function(x, value) {
  UseMethod("charset")
}

#' Set CHARSET
#'
#' @param x A px object
#' @param value A character string
#'
#' @export
charset.px <- function(x, value) {
  validate_px(modify_table1(x, "CHARSET", value))
}


#' @rdname creation_date.px
#' @export
creation_date <- function(x, value) {
  UseMethod("creation_date")
}

#' Set CREATION-DATE
#'
#' @param x A px object
#' @param value A character string
#'
#' @export
creation_date.px <- function(x, value = format(Sys.time(), "%Y%m%d %H:%M")) {
  validate_px(modify_table1(x, "CREATION-DATE", value))
}


#' @rdname matrix.px
#' @export
matrix <- function(x, value) {
  UseMethod("matrix")
}

#' Set MATRIX
#'
#' @param x A px object
#' @param value A character string
#'
#' @export
matrix.px <- function(x, value) {
  validate_px(modify_table1(x, "MATRIX", value))
}


#' @rdname decimals.px
#' @export
decimals <- function(x, value) {
  UseMethod("decimals")
}

#' Set DECIMALS
#'
#' @param x A px object
#' @param value A character string
#'
#' @export
decimals.px <- function(x, value) {
  validate_px(modify_table1(x, "DECIMALS", value))
}
