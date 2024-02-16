#' Handle VALUENOTE and VALUENOTEX
#'
#' @rdname valuenote.px
#' @param type A character string, VALUENOTE or VALUENOTEX
handle_valuenotes <- function(x, value, type) {
  if (missing(value)) {
    result <- get_codelists2_value(x, type)

    if (nrow(result) == 0) {
      return(NULL)
    } else {
      return(result)
    }
  } else if (is.null(value)) {
    x$codelists2[[type]] <- NA
    return(x)
  }

  validate_px(modify_codelists2(x, type, value))
}

#' @rdname valuenote.px
#' @export
valuenote <- function(x, value) {
  UseMethod("valuenote")
}

#' VALUENOTE
#'
#' Inspect or change VALUENOTE.
#'
#' @param x A px object
#' @param value Optional. A data frame with the column 'valuenote' and one or
#' more of the columns: 'variable-code', 'code', and 'language'. If value is
#' missing, all current VALUENOTEs are returned. If NULL, all VALUENOTEs are
#' removed.
#'
#' @export
valuenote.px <- function(x, value) {
  handle_valuenotes(x, value, "valuenote")
}

#' @rdname valuenotex.px
#' @export
valuenotex <- function(x, value) {
  UseMethod("valuenotex")
}

#' VALUENOTEX
#'
#' Inspect or change VALUENOTEX.
#'
#' @param x A px object
#' @param value Optional. A data frame with the column 'valuenotex' and one or
#' more of the columns: 'variable-code', 'code', and 'language'. If value is
#' missing, all current VALUENOTEXs are returned. If NULL, all VALUENOTEXs are
#' removed.
#'
#' @export
valuenotex.px <- function(x, value) {
  handle_valuenotes(x, value, "valuenotex")
}
