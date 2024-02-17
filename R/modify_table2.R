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
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current LAST-UPDATED is returned. If
#' NULL, LAST-UPDATED is removed.
#'
#' @export
last_updated.px <- function(x, value) {
  handle_table2_keyword(x, value, "LAST_UPDATED")
}


#' @rdname contents.px
#' @export
contents <- function(x, value) {
  UseMethod("contents")
}

#' CONTENTS
#'
#' Inspect or change CONTENTS. CONTENTS cannot be removed because it is a
#' mandatory keyword.
#'
#' @param x A px object
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current CONTENTS is returned.
#'
#' @export
contents.px <- function(x, value) {
  handle_table2_keyword(x, value, "CONTENTS")
}

# description method and px function
#' @rdname description.px
#' @export
description <- function(x, value) {
  UseMethod("description")
}

#' DESCRIPTION
#'
#' Inspect or change DESCRIPTION. DESCRIPTION cannot be removed because it is a
#' mandatory keyword.
#'
#' @param x A px object
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current DESCRIPTION is returned.
#'
#' @export
description.px <- function(x, value) {
  handle_table2_keyword(x, value, "DESCRIPTION")
}

#' @rdname subject_area.px
#' @export
subject_area <- function(x, value) {
  UseMethod("subject_area")
}

#' SUBJECT-AREA
#'
#' Inspect or change SUBJECT-AREA. SUBJECT-AREA cannot be removed because it is a
#' mandatory keyword.
#'
#' @param x A px object
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current SUBJECT-AREA is returned.
#'
#' @export
subject_area.px <- function(x, value) {
  handle_table2_keyword(x, value, "SUBJECT_AREA")
}


#' @rdname title.px
#' @export
title <- function(x, value) {
  UseMethod("title")
}

#' TITLE
#'
#' Inspect or change TITLE. TITLE cannot be removed because it is a mandatory
#' keyword.
#'
#' @param x A px object
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current TITLE is returned.
#'
#' @export
title.px <- function(x, value) {
  handle_table2_keyword(x, value, "TITLE")
}

#' @rdname units.px
#' @export
units <- function(x, value) {
  UseMethod("units")
}

#' UNITS
#'
#' Inspect or change UNITS. UNITS cannot be removed because it is a mandatory
#' keyword.
#'
#' @param x A px object
#' @param value Optional. A character string to set the value for all languages,
#' or a data frame with columns 'language' and 'value' to set it for specific
#' languages. If 'value' is missing, the current UNITS is returned.
#'
#' @export
units.px <- function(x, value) {
  handle_table2_keyword(x, value, "UNITS")
}
