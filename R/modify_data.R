#' @rdname px_data.px
#' @export
px_data <- function(x, value) {
  UseMethod("px_data")
}

#' @eval add_doc_keyword_function_intro("DATA")
#' @param value A data frame
#' @eval add_return_px_or_df()
#' @export
px_data.px <- function(x, value) {
  if (missing(value)) {
    return(x$data)
  } else {
    x$data <- value
  }

  return(validate_px(x))
}
