get_table2_value <- function(x, keyword) {
  x$table2 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::select(language, value)
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
  if (missing(value)) {
    last_updated <- get_table2_value(x, "LAST-UPDATED")

    if (nrow(last_updated) == 0) {
      last_updated <- NULL
    } else if (nrow(last_updated) == 1) {
      last_updated <- last_updated$value
    }

    return(last_updated)
  } else if (is.null(value)) {
    return(remove_keyword_table2(x, "LAST-UPDATED"))
  }

  validate_px(modify_table2(x, "LAST-UPDATED", value))
}


