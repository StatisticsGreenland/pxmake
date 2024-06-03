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

#' @rdname px_contents.px
#' @export
px_contents <- function(x, value) {
  UseMethod("px_contents")
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
px_contents.px <- function(x, value) {
  handle_table2_keyword(x, value, "CONTENTS")
}


#' @rdname px_baseperiod.px
#' @export
px_baseperiod <- function(x, value) {
  UseMethod("px_baseperiod")
}

#' @inherit px_contents.px
#' @title BASEPERIOD
#' @description `r table_description("BASEPERIOD")`
#' @param value `r table2_param_value("BASEPERIOD")`
#' @export
px_baseperiod.px <- function(x, value) {
  handle_table2_keyword(x, value, "BASEPERIOD")
}


#' @rdname px_cfprices.px
#' @export
px_cfprices <- function(x, value) {
  UseMethod("px_cfprices")
}

#' @inherit px_contents.px
#' @title CFPRICES
#' @description `r table_description("CFPRICES")`
#' @param value `r table2_param_value("CFPRICES")`
#' @export
px_cfprices.px <- function(x, value) {
  handle_table2_keyword(x, value, "CFPRICES")
}


#' @rdname px_contact.px
#' @export
px_contact <- function(x, value) {
  UseMethod("px_contact")
}

#' @inherit px_contents.px
#' @title CONTACT
#' @description `r table_description("CONTACT")`
#' @param value `r table2_param_value("CONTACT")`
#' @export
px_contact.px <- function(x, value) {
  handle_table2_keyword(x, value, "CONTACT")
}


#' @rdname px_description.px
#' @export
px_description <- function(x, value) {
  UseMethod("px_description")
}

#' @inherit px_contents.px
#' @title DESCRIPTION
#' @description `r table_description("DESCRIPTION")`
#' @param value `r table2_param_value("DESCRIPTION")`
#' @export
px_description.px <- function(x, value) {
  handle_table2_keyword(x, value, "DESCRIPTION")
}


#' @rdname px_infofile.px
#' @export
px_infofile <- function(x, value) {
  UseMethod("px_infofile")
}

#' @inherit px_contents.px
#' @title INFOFILE
#' @description `r table_description("INFOFILE")`
#' @param value `r table2_param_value("INFOFILE")`
#' @export
px_infofile.px <- function(x, value) {
  handle_table2_keyword(x, value, "INFOFILE")
}


#' @rdname px_last_updated.px
#' @export
px_last_updated <- function(x, value) {
  UseMethod("px_last_updated")
}

#' @inherit px_contents.px
#' @title LAST-UPDATED
#' @description `r table_description("LAST-UPDATED")`
#' @param value `r table2_param_value("LAST-UPDATED")`
#' @export
px_last_updated.px <- function(x, value) {
  handle_table2_keyword(x, value, "LAST-UPDATED")
}


#' @rdname px_link.px
#' @export
px_link <- function(x, value) {
  UseMethod("px_link")
}

#' @inherit px_contents.px
#' @title LINK
#' @description `r table_description("LINK")`
#' @param value `r table2_param_value("LINK")`
#' @export
px_link.px <- function(x, value) {
  handle_table2_keyword(x, value, "LINK")
}


#' @rdname px_map.px
#' @export
px_map <- function(x, value) {
  UseMethod("px_map")
}

#' @inherit px_contents.px
#' @title MAP
#' @description `r table_description("MAP")`
#' @param value `r table2_param_value("MAP")`
#' @export
px_map.px <- function(x, value) {
  handle_table2_keyword(x, value, "MAP")
}


#' @rdname px_stockfa.px
#' @export
px_stockfa <- function(x, value) {
  UseMethod("px_stockfa")
}

#' @inherit px_contents.px
#' @title STOCKFA
#' @description `r table_description("STOCKFA")`
#' @param value `r table2_param_value("STOCKFA")`
#' @export
px_stockfa.px <- function(x, value) {
  handle_table2_keyword(x, value, "STOCKFA")
}


#' @rdname px_subject_area.px
#' @export
px_subject_area <- function(x, value) {
  UseMethod("px_subject_area")
}

#' @inherit px_contents.px
#' @title SUBJECT-AREA
#' @description `r table_description("SUBJECT-AREA")`
#' @param value `r table2_param_value("SUBJECT-AREA")`
#' @export
px_subject_area.px <- function(x, value) {
  handle_table2_keyword(x, value, "SUBJECT-AREA")
}


#' @rdname px_title.px
#' @export
px_title <- function(x, value) {
  UseMethod("px_title")
}

#' @inherit px_contents.px
#' @title TITLE
#' @description `r table_description("TITLE")`
#' @param value `r table2_param_value("TITLE")`
#' @export
px_title.px <- function(x, value) {
  handle_table2_keyword(x, value, "TITLE")
}


#' @rdname px_units.px
#' @export
px_units <- function(x, value) {
  UseMethod("px_units")
}

#' @inherit px_contents.px
#' @title UNITS
#' @description `r table_description("UNITS")`
#' @param value `r table2_param_value("UNITS")`
#' @export
px_units.px <- function(x, value) {
  handle_table2_keyword(x, value, "UNITS")
}
