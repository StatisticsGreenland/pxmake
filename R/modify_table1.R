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

#' @rdname px_charset.px
#' @export
px_charset <- function(x, value) {
  UseMethod("px_charset")
}

#' @eval add_documentation_table1("CHARSET")
px_charset.px <- function(x, value) {
  handle_table1_keyword(x, value, "CHARSET")
}


#' @rdname px_aggregallowed.px
#' @export
px_aggregallowed <- function(x, value) {
  UseMethod("px_aggregallowed")
}

#' @eval add_documentation_table1("AGGREGALLOWED")
px_aggregallowed.px <- function(x, value) {
  handle_table1_keyword(x, value, "AGGREGALLOWED")
}


#' @rdname px_autopen.px
#' @export
px_autopen <- function(x, value) {
  UseMethod("px_autopen")
}

#' @eval add_documentation_table1("AUTOPEN")
px_autopen.px <- function(x, value) {
  handle_table1_keyword(x, value, "AUTOPEN")
}


#' @rdname px_axis_version.px
#' @export
px_axis_version <- function(x, value) {
  UseMethod("px_axis_version")
}

#' @eval add_documentation_table1("AXIS-VERSION")
px_axis_version.px <- function(x, value) {
  handle_table1_keyword(x, value, "AXIS-VERSION")
}


#' @rdname px_codepage.px
#' @export
px_codepage <- function(x, value) {
  UseMethod("px_codepage")
}

#' @eval add_documentation_table1("CODEPAGE")
px_codepage.px <- function(x, value) {
  handle_table1_keyword(x, value, "CODEPAGE")
}


#' @rdname px_confidential.px
#' @export
px_confidential <- function(x, value) {
  UseMethod("px_confidential")
}

#' @eval add_documentation_table1("CONFIDENTIAL")
px_confidential.px <- function(x, value) {
  handle_table1_keyword(x, value, "CONFIDENTIAL")
}


#' @rdname px_copyright.px
#' @export
px_copyright <- function(x, value) {
  UseMethod("px_copyright")
}

#' @eval add_documentation_table1("COPYRIGHT")
px_copyright.px <- function(x, value) {
  handle_table1_keyword(x, value, "COPYRIGHT")
}


#' @rdname px_creation_date.px
#' @export
px_creation_date <- function(x, value) {
  UseMethod("px_creation_date")
}

#' @eval add_documentation_table1("CREATION-DATE")
px_creation_date.px <- function(x, value) {
  handle_table1_keyword(x, value, "CREATION-DATE")
}


#' @rdname px_decimals.px
#' @export
px_decimals <- function(x, value) {
  UseMethod("px_decimals")
}

#' @eval add_documentation_table1("DECIMALS")
px_decimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "DECIMALS")
}


#' @rdname px_descriptiondefault.px
#' @export
px_descriptiondefault <- function(x, value) {
  UseMethod("px_descriptiondefault")
}

#' @eval add_documentation_table1("DESCRIPTIONDEFAULT")
px_descriptiondefault.px <- function(x, value) {
  handle_table1_keyword(x, value, "DESCRIPTIONDEFAULT")
}


#' @rdname px_matrix.px
#' @export
px_matrix <- function(x, value) {
  UseMethod("px_matrix")
}

#' @eval add_documentation_table1("MATRIX")
px_matrix.px <- function(x, value) {
  handle_table1_keyword(x, value, "MATRIX")
}


#' @rdname px_next_update.px
#' @export
px_next_update <- function(x, value) {
  UseMethod("px_next_update")
}

#' @eval add_documentation_table1("NEXT-UPDATE")
px_next_update.px <- function(x, value) {
  handle_table1_keyword(x, value, "NEXT-UPDATE")
}


#' @rdname px_showdecimals.px
#' @export
px_showdecimals <- function(x, value) {
  UseMethod("px_showdecimals")
}

#' @eval add_documentation_table1("SHOWDECIMALS")
px_showdecimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "SHOWDECIMALS")
}


#' @rdname px_subject_code.px
#' @export
px_subject_code <- function(x, value) {
  UseMethod("px_subject_code")
}

#' @eval add_documentation_table1("SUBJECT-CODE")
px_subject_code.px <- function(x, value) {
  handle_table1_keyword(x, value, "SUBJECT-CODE")
}


#' @rdname px_tableid.px
#' @export
px_tableid <- function(x, value) {
  UseMethod("px_tableid")
}

#' @eval add_documentation_table1("TABLEID")
px_tableid.px <- function(x, value) {
  handle_table1_keyword(x, value, "TABLEID")
}


#' @rdname px_update_frequency.px
#' @export
px_update_frequency <- function(x, value) {
  UseMethod("px_update_frequency")
}

#' @eval add_documentation_table1("UPDATE-FREQUENCY")
px_update_frequency.px <- function(x, value) {
  handle_table1_keyword(x, value, "UPDATE-FREQUENCY")
}
