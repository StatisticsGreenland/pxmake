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

#' CHARSET
#'
#' Inspect or change CHARSET.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current CHARSET is
#' returned. If NULL, CHARSET is removed.
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

#' CREATION-DATE
#'
#' Inspect or change CREATION-DATE.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current
#' CREATION-DATE is returned. If NULL, CREATION-DATE is removed.
#'
#' @export
creation_date.px <- function(x, value) {
  handle_table1_keyword(x, value, "CREATION-DATE")
}


#' @rdname matrix.px
#' @export
matrix <- function(x, value) {
  UseMethod("matrix")
}

#' MATRIX
#'
#' Inspect or change MATRIX.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current MATRIX is
#' returned. If NULL, MATRIX is removed.
#'
#' @export
matrix.px <- function(x, value) {
  handle_table1_keyword(x, value, "MATRIX")
}


#' @rdname decimals.px
#' @export
decimals <- function(x, value) {
  UseMethod("decimals")
}

#' DECIMALS
#'
#' Inspect or change DECIMALS. DECIMALS cannot be removed because it is a
#' mandatory keyword.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current DECIMALS
#' is returned. If NULL, DECIMALS is set to its default value.
#'
#' @export
decimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "DECIMALS")
}

#' @rdname next_update.px
#' @export
next_update <- function(x, value) {
  UseMethod("next_update")
}

#' NEXT-UPDATE
#'
#' Inspect or change NEXT-UPDATE.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current
#' NEXT-UPDATE is returned. If NULL, NEXT-UPDATE is removed.
#'
#' @export
next_update.px <- function(x, value) {
  handle_table1_keyword(x, value, "NEXT-UPDATE")
}

#' @rdname subject_code.px
#' @export
subject_code <- function(x, value) {
  UseMethod("subject_code")
}

#' SUBJECT-CODE
#'
#' Inspect or change SUBJECT-CODE.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current
#' SUBJECT-CODE is returned. If NULL, SUBJECT-CODE is removed.
#'
#' @export
subject_code.px <- function(x, value) {
  handle_table1_keyword(x, value, "SUBJECT-CODE")
}
