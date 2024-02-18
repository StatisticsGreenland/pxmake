handle_table2_keyword <- function(x, value, keyword) {
  if (missing(value)) {
    return(get_table2_value(x, keyword))
  } else if (is.null(value)) {
    error_if_mandatory_keyword(x, keyword)

    x <- remove_keyword_table2(x, keyword)
  } else {
    x <- modify_table2(x, keyword, value)
  }

  validate_px(x)
}

#' @rdname contents.px
#' @export
contents <- function(x, value) {
  UseMethod("contents")
}

#' @title CONTENTS
#'
#' @description `r table_description("CONTENTS")`
#'
#' @param x A px object
#' @param value `r table2_param_value("CONTENTS")`
#'
#' @return A px object, a character string, or a data frame.
#'
#' @export
contents.px <- function(x, value) {
  handle_table2_keyword(x, value, "CONTENTS")
}


#' @rdname description.px
#' @export
description <- function(x, value) {
  UseMethod("description")
}

#' @inherit contents.px
#' @title DESCRIPTION
#' @description `r table_description("DESCRIPTION")`
#' @param value `r table2_param_value("DESCRIPTION")`
#' @export
description.px <- function(x, value) {
  handle_table2_keyword(x, value, "DESCRIPTION")
}


#' @rdname last_updated.px
#' @export
last_updated <- function(x, value) {
  UseMethod("last_updated")
}

#' @inherit contents.px
#' @title LAST-UPDATED
#' @description `r table_description("LAST-UPDATED")`
#' @param value `r table2_param_value("LAST-UPDATED")`
#' @export
last_updated.px <- function(x, value) {
  handle_table2_keyword(x, value, "LAST_UPDATED")
}


#' @rdname subject_area.px
#' @export
subject_area <- function(x, value) {
  UseMethod("subject_area")
}

#' @inherit contents.px
#' @title SUBJECT-AREA
#' @description `r table_description("SUBJECT-AREA")`
#' @param value `r table2_param_value("SUBJECT-AREA")`
#' @export
subject_area.px <- function(x, value) {
  handle_table2_keyword(x, value, "SUBJECT-AREA")
}


#' @rdname title.px
#' @export
title <- function(x, value) {
  UseMethod("title")
}

#' @inherit contents.px
#' @title TITLE
#' @description `r table_description("TITLE")`
#' @param value `r table2_param_value("TITLE")`
#' @export
title.px <- function(x, value) {
  handle_table2_keyword(x, value, "TITLE")
}

#' @rdname units.px
#' @export
units <- function(x, value) {
  UseMethod("units")
}

#' @inherit contents.px
#' @title UNITS
#' @description `r table_description("UNITS")`
#' @param value `r table2_param_value("UNITS")`
#' @export
units.px <- function(x, value) {
  handle_table2_keyword(x, value, "UNITS")
}
