handle_table1_keyword <- function(x, value, keyword, validate) {
  if (missing(value)) {
    return(get_table1_value(x, keyword))
  } else if (is.null(value)) {
    error_if_mandatory_keyword(x, keyword)

    x <- remove_keyword_table1(x, keyword)
  } else {
    x <- modify_table1(x, keyword, value)
  }

  return_px(x, validate)
}

#' @rdname px_aggregallowed.px
#' @export
px_aggregallowed <- function(x, value, validate) {
  UseMethod("px_aggregallowed")
}

#' @eval add_documentation_table1("AGGREGALLOWED", "NO")
px_aggregallowed.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "AGGREGALLOWED", validate)
}


#' @rdname px_autopen.px
#' @export
px_autopen <- function(x, value, validate) {
  UseMethod("px_autopen")
}

#' @eval add_documentation_table1("AUTOPEN", "YES")
px_autopen.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "AUTOPEN", validate)
}


#' @rdname px_axis_version.px
#' @export
px_axis_version <- function(x, value, validate) {
  UseMethod("px_axis_version")
}

#' @eval add_documentation_table1("AXIS-VERSION", "2010")
px_axis_version.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "AXIS-VERSION", validate)
}


#' @rdname px_charset.px
#' @export
px_charset <- function(x, value, validate) {
  UseMethod("px_charset")
}

#' @eval add_documentation_table1("CHARSET", "ANSI")
px_charset.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "CHARSET", validate)
}


#' @rdname px_codepage.px
#' @export
px_codepage <- function(x, value, validate) {
  UseMethod("px_codepage")
}

#' @eval add_documentation_table1("CODEPAGE", "utf-8")
#' @details
#' CODEPAGE controls which encoding PX-files are read and stored in.
#' Use `iconvlist()` to see available encodings on your system.
#' @seealso [px_save()]
px_codepage.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "CODEPAGE", validate)
}


#' @rdname px_confidential.px
#' @export
px_confidential <- function(x, value, validate) {
  UseMethod("px_confidential")
}

#' @eval add_documentation_table1("CONFIDENTIAL", "1")
px_confidential.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "CONFIDENTIAL", validate)
}


#' @rdname px_copyright.px
#' @export
px_copyright <- function(x, value, validate) {
  UseMethod("px_copyright")
}

#' @eval add_documentation_table1("COPYRIGHT", "YES")
px_copyright.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "COPYRIGHT", validate)
}


#' @rdname px_creation_date.px
#' @export
px_creation_date <- function(x, value, validate) {
  UseMethod("px_creation_date")
}

#' @eval add_documentation_table1("CREATION-DATE", "19960612 14:20")
px_creation_date.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "CREATION-DATE", validate)
}


#' @rdname px_decimals.px
#' @export
px_decimals <- function(x, value, validate) {
  UseMethod("px_decimals")
}

#' @eval add_documentation_table1("DECIMALS", "3")
px_decimals.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "DECIMALS", validate)
}


#' @rdname px_descriptiondefault.px
#' @export
px_descriptiondefault <- function(x, value, validate) {
  UseMethod("px_descriptiondefault")
}

#' @eval add_documentation_table1("DESCRIPTIONDEFAULT", "YES")
px_descriptiondefault.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "DESCRIPTIONDEFAULT", validate)
}


#' @rdname px_matrix.px
#' @export
px_matrix <- function(x, value, validate) {
  UseMethod("px_matrix")
}

#' @eval add_documentation_table1("MATRIX", "POPULATION")
px_matrix.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "MATRIX", validate)
}


#' @rdname px_next_update.px
#' @export
px_next_update <- function(x, value, validate) {
  UseMethod("px_next_update")
}

#' @eval add_documentation_table1("NEXT-UPDATE", "20240621 15:55")
px_next_update.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "NEXT-UPDATE", validate)
}


#' @rdname px_showdecimals.px
#' @export
px_showdecimals <- function(x, value, validate) {
  UseMethod("px_showdecimals")
}

#' @eval add_documentation_table1("SHOWDECIMALS", "2")
px_showdecimals.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "SHOWDECIMALS", validate)
}


#' @rdname px_subject_code.px
#' @export
px_subject_code <- function(x, value, validate) {
  UseMethod("px_subject_code")
}

#' @eval add_documentation_table1("SUBJECT-CODE", "POP")
px_subject_code.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "SUBJECT-CODE", validate)
}


#' @rdname px_tableid.px
#' @export
px_tableid <- function(x, value, validate) {
  UseMethod("px_tableid")
}

#' @eval add_documentation_table1("TABLEID", "POPGL")
px_tableid.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "TABLEID", validate)
}


#' @rdname px_update_frequency.px
#' @export
px_update_frequency <- function(x, value, validate) {
  UseMethod("px_update_frequency")
}

#' @eval add_documentation_table1("UPDATE-FREQUENCY", "Yearly")
px_update_frequency.px <- function(x, value, validate = TRUE) {
  handle_table1_keyword(x, value, "UPDATE-FREQUENCY", validate)
}
