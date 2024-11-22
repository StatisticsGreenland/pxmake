handle_variables2_keyword <- function(x, value, keyword, validate) {
  colname <- tolower(keyword)

  if (missing(value)) {
    return(get_variables2_value(x, colname))
  } else if (is.null(value)) {
    return(remove_keyword_variables2(x, keyword))
  } else if (is.character(value)) {
    value <- dplyr::tibble(`variable-code` = c(px_stub(x), px_heading(x)),
                           !!colname := value
                           )
  }

  x <- modify_variables2(x, colname, value)

  return_px(x, validate)
}

#' @rdname px_domain.px
#' @export
px_domain <- function(x, value, validate) {
  UseMethod("px_domain")
}

#' @eval add_documentation_variables2("DOMAIN", "aggregation1", "aggregation2", "aggregation3")
px_domain.px <- function(x, value, validate = TRUE) {
  handle_variables2_keyword(x, value, "DOMAIN", validate)
}

#' @rdname px_elimination.px
#' @export
px_elimination <- function(x, value, validate) {
  UseMethod("px_elimination")
}

#' @eval add_documentation_variables2("ELIMINATION", "YES", "All", "Total")
px_elimination.px <- function(x, value, validate = TRUE) {
  handle_variables2_keyword(x, value, "ELIMINATION", validate)
}

#' @rdname px_map.px
#' @export
px_map <- function(x, value, validate) {
  UseMethod("px_map")
}

#' @eval add_documentation_variables2("MAP", "greenland", "cities", "municipalities")
px_map.px <- function(x, value, validate = TRUE) {
  handle_variables2_keyword(x, value, "MAP", validate)
}

#' @rdname px_variable_label.px
#' @export
px_variable_label <- function(x, value, validate) {
  UseMethod("px_variable_label")
}

#' @title Change VARIABLE-LABEL
#' @eval add_documentation_variables2("VARIABLE-LABEL", NA, NA, NA)
#' @description
#' The variable label is the name that is shown in the px file.
#' @examples
#' # Set VARIABLE-LABEL for individual variables
#' library(tibble)
#' x1 <-
#'   px(population_gl) |>
#'   px_variable_label(tribble(~`variable-code`, ~`variable-label`,
#'                             'gender',         'Gender',
#'                             'age',            'Age'))
#' px_variable_label(x1)
#'
#' # Set VARIABLE-LABEL for individual languages
#' x2 <-
#'   x1 %>%
#'   px_languages(c('en', 'kl')) |>
#'   px_variable_label(tribble(~`variable-code`, ~language, ~`variable-label`,
#'                             'gender',         'en',      'Gender',
#'                             'gender',         'kl',      'Suiaassuseq',
#'                             'age',            'en',      'Age',
#'                             'age',            'kl',      'Ukiut'))
#' px_variable_label(x2)
#'
#' # Remove VARIABLE-LABEL
#' x3 <- px_variable_label(x2, NULL)
#' px_variable_label(x3)
px_variable_label.px <- function(x, value, validate = TRUE) {
  handle_variables2_keyword(x, value, "variable-label", validate)
}
