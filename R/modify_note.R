handle_notes <- function(x, value, keyword) {
  colname <- tolower(keyword)
  error_msg <- stringr::str_glue("Argument 'value' has wrong format. See ?{colname}.")

  if (missing(value)) {
    value_table2 <- get_table2_value(x, keyword)
    value_variables2 <- get_variables2_value(x, colname)

    if (is.null(value_table2)) {
      return(value_variables2)
    } else if (is.null(value_variables2)) {
      return(value_table2)
    } else {
      return(list(value_table2, value_variables2))
    }
  }

  if (is.null(value)) {
    x <- remove_keyword_table2(x, keyword)
    x$table2[[colname]] <- NULL
  } else if (is.character(value)) {
    x <- modify_table2(x, keyword, value)
  } else if (is.data.frame(value)) {
    if (all(c("language", "value") %in% colnames(value))) {
      x <- modify_table2(x, keyword, value)
    } else if (all(c("variable-code", colname) %in% colnames(value))) {
      x <- modify_variables2(x, colname, value)
    } else {
      error(error_msg)
    }
  } else if (is.list(value)) {
    for (i in seq_along(value)) {
      x <- get(paste0(colname, ".px"))(x, value[[i]])
    }
  } else {
    error(error_msg)
  }

  validate_px(x)
}


#' @rdname note.px
#' @export
note <- function(x, value) {
  UseMethod("note")
}

#' @title NOTE
#'
#' @description `r note_description("NOTE")`
#'
#' @param x A px object
#' @param value `r note_param_value("NOTE")`
#'
#' @return A px object, a character string, a data frame, or a list of character
#' strings and/or data frames.
#'
#' @export
note.px <- function(x, value) {
  handle_notes(x, value, "NOTE")
}


#' @rdname notex.px
#' @export
notex <- function(x, value) {
  UseMethod("notex")
}

#' @inherit note.px
#' @title NOTEX
#' @description `r note_description("NOTEX")`
#' @param value `r note_param_value("NOTEX")`
#' @export
notex.px <- function(x, value) {
  handle_notes(x, value, "NOTEX")
}
