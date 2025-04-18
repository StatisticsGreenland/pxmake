handle_table2_keyword <- function(x, value, keyword, validate) {
  if (missing(value)) {
    return(get_table2_value(x, keyword))
  } else if (is.null(value)) {
    error_if_mandatory_keyword(x, keyword)

    x <- remove_keyword_table2(x, keyword)
  } else {
    x <- modify_table2(x, keyword, value)
  }

  return_px(x, validate)
}

#' @rdname px_baseperiod.px
#' @export
px_baseperiod <- function(x, value, validate) {
  UseMethod("px_baseperiod")
}

#' @eval add_documentation_table2("BASEPERIOD", "year", "ukioq")
px_baseperiod.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "BASEPERIOD", validate)
}


#' @rdname px_cfprices.px
#' @export
px_cfprices <- function(x, value, validate) {
  UseMethod("px_cfprices")
}

#' @eval add_documentation_table2("CFPRICES", "C", "F")
px_cfprices.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "CFPRICES", validate)
}


#' @rdname px_contact.px
#' @export
px_contact <- function(x, value, validate) {
  UseMethod("px_contact")
}

#' @eval add_documentation_table2("CONTACT", "Johan Ejstrud", "Lars Pedersen")
px_contact.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "CONTACT", validate)
}


#' @rdname px_contents.px
#' @export
px_contents <- function(x, value, validate) {
  UseMethod("px_contents")
}

#' @eval add_documentation_table2("CONTENTS", "Population", "Innuttaasut")
px_contents.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "CONTENTS", validate)
}


#' @rdname px_description.px
#' @export
px_description <- function(x, value, validate) {
  UseMethod("px_description")
}

#' @eval add_documentation_table2("DESCRIPTION", "Population", "Innuttaasut")
#' @seealso [px_title()]
px_description.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "DESCRIPTION", validate)
}


#' @rdname px_infofile.px
#' @export
px_infofile <- function(x, value, validate) {
  UseMethod("px_infofile")
}

#' @eval add_documentation_table2("INFOFILE", "infofile_en", "infofile_kl")
px_infofile.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "INFOFILE", validate)
}


#' @rdname px_last_updated.px
#' @export
px_last_updated <- function(x, value, validate) {
  UseMethod("px_last_updated")
}

#' @eval add_documentation_table2("LAST-UPDATED",  "17070501 15:55", "20080621 15:55")
px_last_updated.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "LAST-UPDATED", validate)
}


#' @rdname px_link.px
#' @export
px_link <- function(x, value, validate) {
  UseMethod("px_link")
}

#' @eval add_documentation_table2("LINK", "https://stat.gl/?lang=en", "https://stat.gl/")
px_link.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "LINK", validate)
}


#' @rdname px_source.px
#' @export
px_source <- function(x, value, validate) {
  UseMethod("px_source")
}

#' @eval add_documentation_table2("SOURCE", "Statistics Greenland", "Naatsorsueqqissaartarfik")
px_source.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "SOURCE", validate)
}


#' @rdname px_stockfa.px
#' @export
px_stockfa <- function(x, value, validate) {
  UseMethod("px_stockfa")
}

#' @eval add_documentation_table2("STOCKFA", "S", "F")
px_stockfa.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "STOCKFA", validate)
}


#' @rdname px_subject_area.px
#' @export
px_subject_area <- function(x, value, validate) {
  UseMethod("px_subject_area")
}

#' @eval add_documentation_table2("SUBJECT-AREA", "Population", "Innuttaasut")
px_subject_area.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "SUBJECT-AREA", validate)
}


#' @rdname px_title.px
#' @export
px_title <- function(x, value, validate) {
  UseMethod("px_title")
}

#' @eval add_documentation_table2("TITLE", "Population GR", "Innuttaasut KL")
#' @description TITLE can only be removed if DESCRIPTION is set.
#' @seealso [px_description()]
px_title.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "TITLE", validate)
}


#' @rdname px_units.px
#' @export
px_units <- function(x, value, validate) {
  UseMethod("px_units")
}

#' @eval add_documentation_table2("UNITS", "persons", "inuit amerlassusaat")
px_units.px <- function(x, value, validate = TRUE) {
  handle_table2_keyword(x, value, "UNITS", validate)
}
