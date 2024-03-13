handle_variables2_keyword <- function(x, value, keyword) {
  colname <- tolower(keyword)

  if (missing(value)) {
    return(get_variables2_value(x, colname))
  } else if (is.null(value)) {
    return(remove_keyword_variables2(x, keyword))
  } else {
    x <- modify_variables2(x, colname, value)
  }
}

#' @rdname domain.px
#' @export
domain <- function(x, value) {
  UseMethod("domain")
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
domain.px <- function(x, value) {
  handle_variables2_keyword(x, value, "DOMAIN")
}

#' @rdname elimination.px
#' @export
elimination <- function(x, value) {
  UseMethod("elimination")
}

#' @inherit domain.px
#' @title ELIMINATION
#' @description `r table_description("ELIMINATION")`
#' @param value `r variables2_param_value("ELIMINATION")`
#' @export
elimination.px <- function(x, value) {
  handle_variables2_keyword(x, value, "ELIMINATION")
}
