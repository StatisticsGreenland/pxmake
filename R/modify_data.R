#' @rdname px_data.px
#' @export
px_data <- function(x, value) {
  UseMethod("px_data")
}

#' @eval add_doc_keyword_function_intro("DATA")
#' @param value Optional. A data frame. If missing, the current DATA is returned.
#' If NULL, all data rows are removed.
#' @eval add_return_px_or_df()
#' @export
px_data.px <- function(x, value) {
  if (missing(value)) {
    return(x$data)
  } else if (is.null(value)) {
    x$data <- dplyr::filter(x$data, FALSE)
  } else {
    x$data <- value
  }

  return(validate_px(x))
}
