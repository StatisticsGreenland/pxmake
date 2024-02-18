handle_table1_keyword <- function(x, value, keyword) {
  if (missing(value)) {
    return(get_table1_value(x, keyword))
  } else if (is.null(value)) {
    error_if_mandatory_keyword(x, keyword)

    x <- remove_keyword_table1(x, keyword)
  } else {
    x <- modify_table1(x, keyword, value)
  }

  validate_px(x)
}

#' @rdname charset.px
#' @export
charset <- function(x, value) {
  UseMethod("charset")
}

#' @title CHARSET
#'
#' @description `r table_description("CHARSET")`
#'
#' @param x A px object
#' @param value `r table1_param_value("CHARSET")`
#'
#' @return A px object or a character string
#'
#' @export
charset.px <- function(x, value) {
  handle_table1_keyword(x, value, "CHARSET")
}


#' @rdname creation_date.px
#' @export
creation_date <- function(x, value) {
  UseMethod("creation_date")
}

#' @inherit charset.px
#' @title CREATION-DATE
#' @description `r table_description("CREATION-DATE")`
#' @param value `r table1_param_value("CREATION-DATE")`
#' @export
creation_date.px <- function(x, value) {
  handle_table1_keyword(x, value, "CREATION-DATE")
}


#' @rdname decimals.px
#' @export
decimals <- function(x, value) {
  UseMethod("decimals")
}

#' @inherit charset.px
#' @title DECIMALS
#' @description `r table_description("DECIMALS")`
#' @param value `r table1_param_value("DECIMALS")`
#' @export
decimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "DECIMALS")
}


#' @rdname matrix.px
#' @export
matrix <- function(x, value) {
  UseMethod("matrix")
}

#' @inherit charset.px
#' @title MATRIX
#' @description `r table_description("MATRIX")`
#' @param value `r table1_param_value("MATRIX")`
#' @export
matrix.px <- function(x, value) {
  handle_table1_keyword(x, value, "MATRIX")
}


#' @rdname next_update.px
#' @export
next_update <- function(x, value) {
  UseMethod("next_update")
}

#' @inherit charset.px
#' @title NEXT-UPDATE
#' @description `r table_description("NEXT-UPDATE")`
#' @param value `r table1_param_value("NEXT-UPDATE")`
#' @export
next_update.px <- function(x, value) {
  handle_table1_keyword(x, value, "NEXT-UPDATE")
}


#' @rdname subject_code.px
#' @export
subject_code <- function(x, value) {
  UseMethod("subject_code")
}

#' @inherit charset.px
#' @title SUBJECT-CODE
#' @description `r table_description("SUBJECT-CODE")`
#' @param value `r table1_param_value("SUBJECT-CODE")`
#' @export
subject_code.px <- function(x, value) {
  handle_table1_keyword(x, value, "SUBJECT-CODE")
}
