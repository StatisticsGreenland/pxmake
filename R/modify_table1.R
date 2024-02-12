modify_table1 <- function(x, keyword, value) {
  x$table1 <- modify_or_add_row(x$table1, "keyword", keyword, "value", value)
  return(x)
}

remove_keyword_table1 <- function(x, keyword) {
  x$table1 <- x$table1 %>%
    dplyr::filter(keyword != !!keyword)

  return(x)
}

get_table1_value <- function(x, keyword) {
  x$table1 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::pull(value)
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
  if (missing(value)) {
    return(get_table1_value(x, "CHARSET"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "CHARSET"))
  }

  validate_px(modify_table1(x, "CHARSET", value))
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
  if (missing(value)) {
    return(get_table1_value(x, "CREATION-DATE"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "CREATION-DATE"))
  }

  validate_px(modify_table1(x, "CREATION-DATE", value))
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
  if (missing(value)) {
    return(get_table1_value(x, "MATRIX"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "MATRIX"))
  }

  validate_px(modify_table1(x, "MATRIX", value))
}


#' @rdname decimals.px
#' @export
decimals <- function(x, value) {
  UseMethod("decimals")
}

#' DECIMALS
#'
#' Inspect or change DECIMALS.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current DECIMALS
#' is returned. If NULL, DECIMALS is removed.
#'
#' @export
decimals.px <- function(x, value) {
  if (missing(value)) {
    return(get_table1_value(x, "DECIMALS"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "DECIMALS"))
  }

  validate_px(modify_table1(x, "DECIMALS", value))
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
  if (missing(value)) {
    return(get_table1_value(x, "NEXT-UPDATE"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "NEXT-UPDATE"))
  }

  validate_px(modify_table1(x, "NEXT-UPDATE", value))
}

#' @rdname last_updated.px
#' @export
last_updated <- function(x, value) {
  UseMethod("last_updated")
}

#' LAST-UPDATED
#'
#' Inspect or change LAST-UPDATED.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current
#' LAST-UPDATED is returned. If NULL, LAST-UPDATED is removed.
#'
#' @export
last_updated.px <- function(x, value) {
  if (missing(value)) {
    return(get_table1_value(x, "LAST-UPDATED"))
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "LAST-UPDATED"))
  }

  validate_px(modify_table1(x, "LAST-UPDATED", value))
}
