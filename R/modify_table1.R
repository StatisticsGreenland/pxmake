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


#' @rdname aggregallowed.px
#' @export
aggregallowed <- function(x, value) {
  UseMethod("aggregallowed")
}

#' @inherit charset.px
#' @title AGGREGALLOWED
#' @description `r table_description("AGGREGALLOWED")`
#' @param value `r table1_param_value("AGGREGALLOWED")`
#' @export
aggregallowed.px <- function(x, value) {
  handle_table1_keyword(x, value, "AGGREGALLOWED")
}


#' @rdname autopen.px
#' @export
autopen <- function(x, value) {
  UseMethod("autopen")
}

#' @inherit charset.px
#' @title AUTOPEN
#' @description `r table_description("AUTOPEN")`
#' @param value `r table1_param_value("AUTOPEN")`
#' @export
autopen.px <- function(x, value) {
  handle_table1_keyword(x, value, "AUTOPEN")
}


#' @rdname axis_version.px
#' @export
axis_version <- function(x, value) {
  UseMethod("axis_version")
}

#' @inherit charset.px
#' @title AXIS-VERSION
#' @description `r table_description("AXIS-VERSION")`
#' @param value `r table1_param_value("AXIS-VERSION")`
#' @export
axis_version.px <- function(x, value) {
  handle_table1_keyword(x, value, "AXIS-VERSION")
}


#' @rdname codepage.px
#' @export
codepage <- function(x, value) {
  UseMethod("codepage")
}

#' @inherit charset.px
#' @title CODEPAGE
#' @description `r table_description("CODEPAGE")`
#' @param value `r table1_param_value("CODEPAGE")`
#' @export
codepage.px <- function(x, value) {
  handle_table1_keyword(x, value, "CODEPAGE")
}


#' @rdname confidential.px
#' @export
confidential <- function(x, value) {
  UseMethod("confidential")
}

#' @inherit charset.px
#' @title CONFIDENTIAL
#' @description `r table_description("CONFIDENTIAL")`
#' @param value `r table1_param_value("CONFIDENTIAL")`
#' @export
confidential.px <- function(x, value) {
  handle_table1_keyword(x, value, "CONFIDENTIAL")
}


#' @rdname copyright.px
#' @export
copyright <- function(x, value) {
  UseMethod("copyright")
}

#' @inherit charset.px
#' @title COPYRIGHT
#' @description `r table_description("COPYRIGHT")`
#' @param value `r table1_param_value("COPYRIGHT")`
#' @export
copyright.px <- function(x, value) {
  handle_table1_keyword(x, value, "COPYRIGHT")
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


#' @rdname showdecimals.px
#' @export
showdecimals <- function(x, value) {
  UseMethod("showdecimals")
}

#' @inherit charset.px
#' @title SHOWDECIMALS
#' @description `r table_description("SHOWDECIMALS")`
#' @param value `r table1_param_value("SHOWDECIMALS")`
#' @export
showdecimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "SHOWDECIMALS")
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


#' @rdname tableid.px
#' @export
tableid <- function(x, value) {
  UseMethod("tableid")
}

#' @inherit charset.px
#' @title TABLEID
#' @description `r table_description("TABLEID")`
#' @param value `r table1_param_value("TABLEID")`
#' @export
tableid.px <- function(x, value) {
  handle_table1_keyword(x, value, "TABLEID")
}


#' @rdname update_frequency.px
#' @export
update_frequency <- function(x, value) {
  UseMethod("update_frequency")
}

#' @inherit charset.px
#' @title UPDATE-FREQUENCY
#' @description `r table_description("UPDATE-FREQUENCY")`
#' @param value `r table1_param_value("UPDATE-FREQUENCY")`
#' @export
update_frequency.px <- function(x, value) {
  handle_table1_keyword(x, value, "UPDATE-FREQUENCY")
}
