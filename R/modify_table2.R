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


#' @rdname baseperiod.px
#' @export
baseperiod <- function(x, value) {
  UseMethod("baseperiod")
}

#' @inherit contents.px
#' @title BASEPERIOD
#' @description `r table_description("BASEPERIOD")`
#' @param value `r table2_param_value("BASEPERIOD")`
#' @export
baseperiod.px <- function(x, value) {
  handle_table2_keyword(x, value, "BASEPERIOD")
}


#' @rdname contact.px
#' @export
contact <- function(x, value) {
  UseMethod("contact")
}

#' @inherit contents.px
#' @title CONTACT
#' @description `r table_description("CONTACT")`
#' @param value `r table2_param_value("CONTACT")`
#' @export
contact.px <- function(x, value) {
  handle_table2_keyword(x, value, "CONTACT")
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


#' @rdname infofile.px
#' @export
infofile <- function(x, value) {
  UseMethod("infofile")
}

#' @inherit contents.px
#' @title INFOFILE
#' @description `r table_description("INFOFILE")`
#' @param value `r table2_param_value("INFOFILE")`
#' @export
infofile.px <- function(x, value) {
  handle_table2_keyword(x, value, "INFOFILE")
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
  handle_table2_keyword(x, value, "LAST-UPDATED")
}


#' @rdname link.px
#' @export
link <- function(x, value) {
  UseMethod("link")
}

#' @inherit contents.px
#' @title LINK
#' @description `r table_description("LINK")`
#' @param value `r table2_param_value("LINK")`
#' @export
link.px <- function(x, value) {
  handle_table2_keyword(x, value, "LINK")
}


#' @rdname map.px
#' @export
map <- function(x, value) {
  UseMethod("map")
}

#' @inherit contents.px
#' @title MAP
#' @description `r table_description("MAP")`
#' @param value `r table2_param_value("MAP")`
#' @export
map.px <- function(x, value) {
  handle_table2_keyword(x, value, "MAP")
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
