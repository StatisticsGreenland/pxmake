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

#' Get names of pivot variables
#'
#' @param x A px object
#' @param pivot A string, either "STUB", "HEADING" or "FIGURES"
#'
#' @return A character vector of variable codes
pivot_variables <- function(x, pivot) {
  x$variables1 %>%
    dplyr::arrange(order, `variable-code`) %>%
    dplyr::filter(toupper(pivot) == !!pivot) %>%
    dplyr::pull(`variable-code`)
}

#' @rdname stub.px
#' @export
stub <- function(x, variables) {
  UseMethod("stub")
}

#' STUB
#'
#' Inspect or change which variables are used as stubs. The stub order is also
#' changed to the order of the input variables.
#'
#' @param x A px object
#' @param variables Optional. A character vector of variable names to change to
#' STUB. If missing, the current STUB variables are returned.
#'
#' @return A px object or a character vector
#'
#' @seealso \code{\link{heading}} \code{\link{figures}}
#'
#' @export
stub.px <- function(x, variables) {
  if (missing(variables)) {
    return(pivot_variables(x, "STUB"))
  }

  validate_px(change_pivot_variables(x, "STUB", variables))
}

#' @rdname heading.px
#' @export
heading <- function(x, variables) {
  UseMethod("heading")
}

#' HEADING
#'
#' Inspect or change which variables are used as headings. The heading order is
#' also changed to the order of the input variables.
#'
#' @param x A px object
#' @param variables Optional. A character vector of variable names to change to
#' HEADING. If missing, the current HEADING variables are returned.
#'
#' @return A px object or a character vector
#'
#' @seealso \code{\link{stub}} \code{\link{figures}}
#'
#' @export
heading.px <- function(x, variables) {
  if (missing(variables)) {
    return(pivot_variables(x, "HEADING"))
  }

  validate_px(change_pivot_variables(x, "HEADING", variables))
}

#' @rdname figures.px
#' @export
figures <- function(x, variable) {
  UseMethod("figures")
}

#' FIGURES
#'
#' Inspect or change which variable is used as figures. The previous figures
#' variable is changed to STUB.
#'
#' @param x A px object
#' @param variable Optional. Name of variable to use as FIGRUES. If missing, the
#' current FIGURES variable is returned.
#'
#' @return A px object or a character string
#'
#' @seealso \code{\link{stub}} \code{\link{heading}}
#'
#' @export
figures.px <- function(x, variable) {
  if (missing(variable)) {
    return(pivot_variables(x, "FIGURES"))
  }

  error_if_not_exactly_one_figures_variable(variable)

  old_figures_variable <- figures(x)

  x <- change_pivot_variables(x, "FIGURES", variable)

  x <- change_pivot_variables(x, "STUB", old_figures_variable)

  validate_px(x)
}

#' @rdname timeval.px
#' @export
timeval <- function(x, variable) {
  UseMethod("timeval")
}

#' TIMEVAL
#'
#' Inspect or change which variable is used as timeval. There can only be one
#' time variable.
#'
#' @param x A px object
#' @param variable Optional. Name of variable to use as TIME. If missing, the
#' current TIME variable is returned.
#'
#' @return A px object or a character string
#'
#' @export
timeval.px <- function(x, variable) {
  if (missing(variable)) {
    return(x$variables1 %>%
             dplyr::filter(toupper(type) == "TIME") %>%
             dplyr::pull(`variable-code`)
           )
  }

  x$variables1$type <- NA

  x$variables1 <- modify_or_add_row(df = x$variables1,
                                    lookup_column = "variable-code",
                                    lookup_column_values = variable,
                                    modify_column = "type",
                                    new_value = "TIME"
                                    )
  validate_px(x)
}
