#' Change stub/heading variables
#'
#' @param x A px object
#' @param pivot Pivot type (STUB or HEADING)
#' @param variables A character vector of variable codes to change to the pivot
#' type
#'
#' @return A px object
change_pivot_variables <- function(x, pivot, variables) {
  pivot <- toupper(pivot)

  old_pivot_variables <- pivot_variables(x, pivot)

  new_pivot_variables <- unique(c(variables, old_pivot_variables))

  order_df <- dplyr::tibble(`variable-code` = new_pivot_variables,
                            order = 1:length(new_pivot_variables)
                            )

  x$variables1 <-
    modify_or_add_row(df = x$variables1,
                      lookup_column = "variable-code",
                      lookup_column_values = variables,
                      modify_column = "pivot",
                      new_value = pivot
    ) %>%
    dplyr::left_join(order_df, by = "variable-code") %>%
    dplyr::mutate(order = ifelse(toupper(pivot) == pivot, order.y, order.x)) %>%
    dplyr::select(-order.y, -order.x) %>%
    dplyr::arrange(desc(pivot), order)

  return(x)
}

#' @rdname stub.px
#' @export
stub <- function(x, ...) {
  UseMethod("stub")
}

#' Set STUB
#'
#' Set which variables are used as stubs. The order is also changed to the order
#' of the input variables.
#'
#' @param x A px object
#' @param variables A character vector of variable codes to change to STUB.
#'
#' @return A px object
#'
#' @export
stub.px <- function(x, variables) {
  change_pivot_variables(x, "STUB", variables)
}

#' @rdname heading.px
#' @export
heading <- function(x, ...) {
  UseMethod("heading")
}

#' Set HEADING
#'
#' Set which variables are used as headings. The order is also changed to the
#' order of the input variables.
#'
#' @param x A px object
#' @param variables A character vector of variable codes to change to HEADING.
#'
#' @return A px object
#'
#' @export
heading.px <- function(x, variables) {
  change_pivot_variables(x, "HEADING", variables)
}

#' @rdname figures.px
#' @export
figures <- function(x, ...) {
  UseMethod("figures")
}

#' Set FIGURES
#'
#' Set which variable is used as figures. The previous figures variable is
#' changed to STUB
#'
#' @param x A px object
#' @param variable A character vector of variable codes to change to FIGURES.
#'
#' @return A px object
#'
#' @export
figures.px <- function(x, variable) {
  error_if_not_exactly_one_figures_variable(variable)

  current_figures_variable <- figures_variable(x)

  x <- change_pivot_variables(x, "FIGURES", variable)

  x <- change_pivot_variables(x, "STUB", current_figures_variable)

  return(x)
}

#' @rdname timeval.px
#' @export
timeval <- function(x, ...) {
  UseMethod("timeval")
}

#' Set TIMEVAL
#'
#' Set which variable is used as time. There can only be one time variable.
#'
#' @param x A px object
#' @param variable Character, variable code to change to TIMEVAL.
#'
#' @return A px object
#'
#' @export
timeval.px <- function(x, variable) {
  x$variables1$type <- NA

  x$variables1 <- modify_or_add_row(df = x$variables1,
                                    lookup_column = "variable-code",
                                    lookup_column_values = variable,
                                    modify_column = "type",
                                    new_value = "TIME"
                                    )
  return(x)
}
