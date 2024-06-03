handle_acrossnote_keyword <- function(x, value, keyword) {
  if (missing(value)) {
    return(get_acrosscell_value(x, keyword))
  } else if (is.null(value)) {
    x$acrosscell <- dplyr::filter(x$acrosscell, FALSE)
  } else if (nrow(value) ==0) {
    return(x)
  } else {
    x <- modify_acrosscell(x, value, keyword)
  }

  return(validate_px(x))
}

#' @rdname px_cellnote.px
#' @export
px_cellnote <- function(x, value) {
  UseMethod("px_cellnote")
}

#' @title CELLNOTE
#'
#' @description `r table_description("CELLNOTE")`
#'
#' @param x A px object
#' @param value `r acrosscell_param_value("CELLNOTE")`
#'
#' @return A px object, or a data frame.
#'
#' @export
px_cellnote.px <- function(x, value) {
  handle_acrossnote_keyword(x, value, "CELLNOTE")
}

#' @rdname px_cellnotex.px
#' @export
px_cellnotex <- function(x, value) {
  UseMethod("px_cellnotex")
}

#' @inherit px_cellnote.px
#' @title CELLNOTEX
#' @description `r table_description("CELLNOTEX")`
#' @param value `r acrosscell_param_value("CELLNOTEX")`
#' @export
px_cellnotex.px <- function(x, value) {
  handle_acrossnote_keyword(x, value, "CELLNOTEX")
}
