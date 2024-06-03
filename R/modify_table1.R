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
px_charset.px <- function(x, value) {
  handle_table1_keyword(x, value, "CHARSET")
}


#' @rdname px_aggregallowed.px
#' @export
px_aggregallowed <- function(x, value) {
  UseMethod("px_aggregallowed")
}

#' @inherit px_charset.px
#' @title AGGREGALLOWED
#' @description `r table_description("AGGREGALLOWED")`
#' @param value `r table1_param_value("AGGREGALLOWED")`
#' @export
px_aggregallowed.px <- function(x, value) {
  handle_table1_keyword(x, value, "AGGREGALLOWED")
}


#' @rdname px_autopen.px
#' @export
px_autopen <- function(x, value) {
  UseMethod("px_autopen")
}

#' @inherit px_charset.px
#' @title AUTOPEN
#' @description `r table_description("AUTOPEN")`
#' @param value `r table1_param_value("AUTOPEN")`
#' @export
px_autopen.px <- function(x, value) {
  handle_table1_keyword(x, value, "AUTOPEN")
}


#' @rdname px_axis_version.px
#' @export
px_axis_version <- function(x, value) {
  UseMethod("px_axis_version")
}

#' @inherit px_charset.px
#' @title AXIS-VERSION
#' @description `r table_description("AXIS-VERSION")`
#' @param value `r table1_param_value("AXIS-VERSION")`
#' @export
px_axis_version.px <- function(x, value) {
  handle_table1_keyword(x, value, "AXIS-VERSION")
}


#' @rdname px_codepage.px
#' @export
px_codepage <- function(x, value) {
  UseMethod("px_codepage")
}

#' @inherit px_charset.px
#' @title CODEPAGE
#' @description `r table_description("CODEPAGE")`
#' @param value `r table1_param_value("CODEPAGE")`
#' @export
px_codepage.px <- function(x, value) {
  handle_table1_keyword(x, value, "CODEPAGE")
}


#' @rdname px_confidential.px
#' @export
px_confidential <- function(x, value) {
  UseMethod("px_confidential")
}

#' @inherit px_charset.px
#' @title CONFIDENTIAL
#' @description `r table_description("CONFIDENTIAL")`
#' @param value `r table1_param_value("CONFIDENTIAL")`
#' @export
px_confidential.px <- function(x, value) {
  handle_table1_keyword(x, value, "CONFIDENTIAL")
}


#' @rdname px_copyright.px
#' @export
px_copyright <- function(x, value) {
  UseMethod("px_copyright")
}

#' @inherit px_charset.px
#' @title COPYRIGHT
#' @description `r table_description("COPYRIGHT")`
#' @param value `r table1_param_value("COPYRIGHT")`
#' @export
px_copyright.px <- function(x, value) {
  handle_table1_keyword(x, value, "COPYRIGHT")
}


#' @rdname px_creation_date.px
#' @export
px_creation_date <- function(x, value) {
  UseMethod("px_creation_date")
}

#' @inherit px_charset.px
#' @title CREATION-DATE
#' @description `r table_description("CREATION-DATE")`
#' @param value `r table1_param_value("CREATION-DATE")`
#' @export
px_creation_date.px <- function(x, value) {
  handle_table1_keyword(x, value, "CREATION-DATE")
}


#' @rdname px_decimals.px
#' @export
px_decimals <- function(x, value) {
  UseMethod("px_decimals")
}

#' @inherit px_charset.px
#' @title DECIMALS
#' @description `r table_description("DECIMALS")`
#' @param value `r table1_param_value("DECIMALS")`
#' @export
px_decimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "DECIMALS")
}


#' @rdname px_descriptiondefault.px
#' @export
px_descriptiondefault <- function(x, value) {
  UseMethod("px_descriptiondefault")
}

#' @inherit px_charset.px
#' @title DESCRIPTIONDEFAULT
#' @description `r table_description("DESCRIPTIONDEFAULT")`
#' @param value `r table1_param_value("DESCRIPTIONDEFAULT")`
#' @export
px_descriptiondefault.px <- function(x, value) {
  handle_table1_keyword(x, value, "DESCRIPTIONDEFAULT")
}


#' @rdname px_matrix.px
#' @export
px_matrix <- function(x, value) {
  UseMethod("px_matrix")
}

#' @inherit px_charset.px
#' @title MATRIX
#' @description `r table_description("MATRIX")`
#' @param value `r table1_param_value("MATRIX")`
#' @export
px_matrix.px <- function(x, value) {
  handle_table1_keyword(x, value, "MATRIX")
}


#' @rdname px_next_update.px
#' @export
px_next_update <- function(x, value) {
  UseMethod("px_next_update")
}

#' @inherit px_charset.px
#' @title NEXT-UPDATE
#' @description `r table_description("NEXT-UPDATE")`
#' @param value `r table1_param_value("NEXT-UPDATE")`
#' @export
px_next_update.px <- function(x, value) {
  handle_table1_keyword(x, value, "NEXT-UPDATE")
}


#' @rdname px_showdecimals.px
#' @export
px_showdecimals <- function(x, value) {
  UseMethod("px_showdecimals")
}

#' @inherit px_charset.px
#' @title SHOWDECIMALS
#' @description `r table_description("SHOWDECIMALS")`
#' @param value `r table1_param_value("SHOWDECIMALS")`
#' @export
px_showdecimals.px <- function(x, value) {
  handle_table1_keyword(x, value, "SHOWDECIMALS")
}


#' @rdname px_subject_code.px
#' @export
px_subject_code <- function(x, value) {
  UseMethod("px_subject_code")
}

#' @inherit px_charset.px
#' @title SUBJECT-CODE
#' @description `r table_description("SUBJECT-CODE")`
#' @param value `r table1_param_value("SUBJECT-CODE")`
#' @export
px_subject_code.px <- function(x, value) {
  handle_table1_keyword(x, value, "SUBJECT-CODE")
}


#' @rdname px_tableid.px
#' @export
px_tableid <- function(x, value) {
  UseMethod("px_tableid")
}

#' @inherit px_charset.px
#' @title TABLEID
#' @description `r table_description("TABLEID")`
#' @param value `r table1_param_value("TABLEID")`
#' @export
px_tableid.px <- function(x, value) {
  handle_table1_keyword(x, value, "TABLEID")
}


#' @rdname px_update_frequency.px
#' @export
px_update_frequency <- function(x, value) {
  UseMethod("px_update_frequency")
}

#' @inherit px_charset.px
#' @title UPDATE-FREQUENCY
#' @description `r table_description("UPDATE-FREQUENCY")`
#' @param value `r table1_param_value("UPDATE-FREQUENCY")`
#' @export
px_update_frequency.px <- function(x, value) {
  handle_table1_keyword(x, value, "UPDATE-FREQUENCY")
}
