handle_acrossnote_keyword <- function(x, value, keyword, na_to_star, validate) {
  if (missing(value)) {
    return(get_acrosscells_value(x, keyword))
  } else if (is.null(value)) {
    x$acrosscells <- dplyr::filter(x$acrosscells, FALSE)
  } else if (nrow(value) ==0) {
    return(x)
  } else {
    x <- modify_acrosscells(x, value, keyword, na_to_star)
  }

  return_px(x, validate)
}

#' @rdname px_cellnote.px
#' @export
px_cellnote <- function(x, value, na_to_star, validate) {
  UseMethod("px_cellnote")
}

#' @eval add_documentation_acrosscells("CELLNOTE")
px_cellnote.px <- function(x, value, na_to_star = TRUE, validate = TRUE) {
  handle_acrossnote_keyword(x, value, "CELLNOTE", na_to_star, validate)
}

#' @rdname px_cellnotex.px
#' @export
px_cellnotex <- function(x, value, na_to_star, validate) {
  UseMethod("px_cellnotex")
}

#' @eval add_documentation_acrosscells("CELLNOTEX")
px_cellnotex.px <- function(x, value, na_to_star = TRUE, validate = TRUE) {
  handle_acrossnote_keyword(x, value, "CELLNOTEX", na_to_star, validate)
}
