handle_variables2_keyword <- function(x, value, keyword) {
  colname <- tolower(keyword)

  if (missing(value)) {
    return(get_variables2_value(x, colname))
  } else if (is.null(value)) {
    return(remove_keyword_variables2(x, keyword))
  } else {
    x <- modify_variables2(x, colname, value)
  }

  validate_px(x)
}

#' @rdname px_domain.px
#' @export
px_domain <- function(x, value) {
  UseMethod("px_domain")
}

#' @title DOMAIN
#'
#' @description `r table_description("DOMAIN")`
#'
#' @param x A px object
#' @param value `r variables2_param_value("DOMAIN")`
#'
#' @return A px object, a character string, or a data frame.
#'
#' @export
px_domain.px <- function(x, value) {
  handle_variables2_keyword(x, value, "DOMAIN")
}

#' @rdname px_elimination.px
#' @export
px_elimination <- function(x, value) {
  UseMethod("px_elimination")
}

#' @inherit px_domain.px
#' @title ELIMINATION
#' @description `r table_description("ELIMINATION")`
#' @param value `r variables2_param_value("ELIMINATION")`
#' @export
px_elimination.px <- function(x, value) {
  handle_variables2_keyword(x, value, "ELIMINATION")
}


#' @rdname px_variable_label.px
#' @export
px_variable_label <- function(x, value) {
  UseMethod("px_variable_label")
}

#' @inherit px_domain.px
#' @title Change variable label
#' @description
#' `r table_description("variable label")`
#'
#' The variable label is the name that is shown in the px file.
#'
#' @param value `r variables2_param_value("variable-label")`
#' @export
px_variable_label.px <- function(x, value) {
  handle_variables2_keyword(x, value, "variable-label")
}
