handle_acrossnote_keyword <- function(x, value, keyword) {
  if (missing(value)) {
    return(get_acrosscells_value(x, keyword))
  } else if (is.null(value)) {
    x$acrosscells <- dplyr::filter(x$acrosscells, FALSE)
  } else if (nrow(value) ==0) {
    return(x)
  } else {
    x <- modify_acrosscells(x, value, keyword)
  }

  return(validate_px(x))
}

#' @rdname px_cellnote.px
#' @export
px_cellnote <- function(x, value) {
  UseMethod("px_cellnote")
}

#' @eval add_documentation_acrosscells("CELLNOTE")
#' @export
px_cellnote.px <- function(x, value) {
  handle_acrossnote_keyword(x, value, "CELLNOTE")
}

#' @rdname px_cellnotex.px
#' @export
px_cellnotex <- function(x, value) {
  UseMethod("px_cellnotex")
}

#' @eval add_documentation_acrosscells("CELLNOTEX")
#' @export
px_cellnotex.px <- function(x, value) {
  handle_acrossnote_keyword(x, value, "CELLNOTEX")
}
