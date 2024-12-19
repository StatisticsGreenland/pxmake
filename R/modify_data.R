#' @rdname px_data.px
#' @export
px_data <- function(x, value, validate) {
  UseMethod("px_data")
}

#' @eval add_doc_keyword_function_intro("DATA")
#' @param value Optional. A data frame. If missing, the current DATA is returned.
#' If NULL, all data rows are removed.
#' @eval add_return_px_or_df()
#' @eval param_validate()
#'
#' @details
#' It is not recommended to change the data table of a px object with this
#' function, since it does update any metadata.
#'
#' @examples
#' x1 <- px(population_gl)
#'
#' # Print data table
#' px_data(x1)
#'
#' # Change data table (risky business)
#' population_gl_2024 <- subset(population_gl, year == 2024)
#'
#' x2 <- px_data(x1, population_gl_2024)
#'
#' @export
px_data.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(x$data)
  } else if (is.null(value)) {
    x$data <- dplyr::filter(x$data, FALSE)
  } else {
    x$data <- value
  }

  return_px(x, validate)
}
