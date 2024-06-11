#' Change pivot variables
#'
#' @param x A px object
#' @param pivot Pivot type (STUB, HEADING, FIGURES)
#' @param variables A character vector of variable codes to change to the pivot
#' type
#'
#' @return A px object
#' @keywords internal
change_pivot_variables <- function(x, variables, pivot) {
  old_pivot_variables <- get_pivot_variables(x, pivot)

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
    dplyr::mutate(order = ifelse(toupper(pivot) == !!pivot, order.y, order.x)) %>%
    dplyr::select(-order.y, -order.x) %>%
    dplyr::arrange(desc(pivot), order) %>%
    align_data_frames(get_base_variables1())

  new_acrosscells_base <- get_base_acrosscells(c(px_stub(x), px_heading(x)))

  x$acrosscells <-
    x$acrosscells %>%
    align_data_frames(new_acrosscells_base) %>%
    dplyr::select(names(new_acrosscells_base))

  return(x)
}

#' Get names of pivot variables
#'
#' @param x A px object
#' @param pivot A string, either "STUB", "HEADING" or "FIGURES"
#'
#' @return A character vector of variable codes
#' @keywords internal
get_pivot_variables <- function(x, pivot) {
  x$variables1 %>%
    dplyr::filter(toupper(pivot) == !!pivot) %>%
    dplyr::arrange(order, `variable-code`) %>%
    dplyr::pull(`variable-code`)
}

#' @rdname px_stub.px
#' @export
px_stub <- function(x, variables) {
  UseMethod("px_stub")
}

#' @title STUB
#'
#' @description `r description_start("STUB")`
#'
#' @param x A px object
#' @param variables `r pivot_param_variables("STUB")`
#' @return A px object or a character vector
#'
#' @seealso \code{\link{px_heading}} \code{\link{px_figures}}
#'
#' @export
px_stub.px <- function(x, variables) {
  if (missing(variables)) {
    return(get_pivot_variables(x, "STUB"))
  }

  validate_px(change_pivot_variables(x, variables, "STUB"))
}

#' @rdname px_heading.px
#' @export
px_heading <- function(x, variables) {
  UseMethod("px_heading")
}

#' @inherit px_stub.px
#' @title HEADING
#' @description `r description_start("HEADING")`
#' @param variables `r pivot_param_variables("HEADING")`
#' @seealso \code{\link{px_stub}} \code{\link{px_figures}}
#' @export
px_heading.px <- function(x, variables) {
  if (missing(variables)) {
    return(get_pivot_variables(x, "HEADING"))
  }

  validate_px(change_pivot_variables(x, variables, "HEADING"))
}

#' @rdname px_figures.px
#' @export
px_figures <- function(x, variable) {
  UseMethod("px_figures")
}

#' @title Change figures variable
#' @description
#' Inspect or change which variable is used as figures. The previous figures
#' variable is changed to STUB.
#'
#' @param x A px object
#' @param variable Optional. Name of variable to use as FIGRUES. If missing, the
#' current PX_FIGURES variable is returned.
#'
#' @return A px object or a character string
#'
#' @seealso \code{\link{px_stub}} \code{\link{px_heading}}
#'
#' @export
px_figures.px <- function(x, variable) {
  if (missing(variable)) {
    return(get_pivot_variables(x, "FIGURES"))
  }

  error_if_not_exactly_one_figures_variable(variable)

  old_figures_variable <- px_figures(x)

  x <- change_pivot_variables(x, variable, "FIGURES")

  x <- change_pivot_variables(x, old_figures_variable, "STUB")

  validate_px(x)
}

#' @rdname px_timeval.px
#' @export
px_timeval <- function(x, variable) {
  UseMethod("px_timeval")
}

#' TIMEVAL
#'
#' Inspect or change which variable is used as px_timeval. There can only be one
#' time variable.
#'
#' @param x A px object
#' @param variable Optional. Name of variable to use as TIMEVAL. If missing, the
#' current TIMEVAL variable is returned.
#'
#' @return A px object or a character string
#'
#' @export
px_timeval.px <- function(x, variable) {
  if (missing(variable)) {
    return(get_variable1_logic_value(x, "timeval"))
  } else if (is.null(variable)) {
    x$variables1$timeval <- FALSE
  } else {
    x$variables1$timeval <- FALSE

    x <- modify_variables1(x, "timeval",
                           dplyr::tibble(`variable-code` = variable,
                                         timeval = TRUE
                                         )
                           )
  }

  validate_px(x)
}

#' @rdname px_contvariable.px
#' @export
px_contvariable <- function(x, value) {
  UseMethod("px_contvariable")
}

#' @title CONTVARIABLE
#'
#' @description `r table_description("CONTVARIABLE")`. Setting CONTVARIABLE
#' indexes several variables in table2. Removing CONTVARIABLE removes the
#' indexing from table2.
#'
#' @param x A px object
#' @param value Optional. A character string with the name of the variable to
#' use as CONTVARIABLE. If missing, the current CONTVARIABLE is returned. If
#' NULL, CONTVARIABLE is removed.
#'
#' @return A px object or a character string.
#'
#' @export
px_contvariable.px <- function(x, value) {
  if (missing(value)) {
    return(get_variable1_logic_value(x, "contvariable"))
  } else if (is.null(value)) {
    x$variables1$contvariable <- FALSE

    previously_indexed_by_contvariable <-
     x$table2 %>%
     dplyr::filter(keyword %in% keywords_indexed_by_contvariable()) %>%
     dplyr::group_by(keyword, language) %>%
     dplyr::slice(1)

    x$table2 <-
      x$table2 %>%
      dplyr::filter(! keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::bind_rows(previously_indexed_by_contvariable) %>%
      dplyr::mutate(code = NA_character_)
  } else {

    x$variables1$contvariable <- FALSE

    x <- modify_variables1(x, "contvariable",
                           dplyr::tibble(`variable-code` = value,
                                         contvariable = TRUE
                                         )
                           )


    contvariable_codes <- unique(x$data[[value]])

    indexed_by_contvariable <-
      x$table2 %>%
      dplyr::filter(keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::select(-code) %>%
      tidyr::crossing(code = contvariable_codes)

    x$table2 <-
      x$table2 %>%
      dplyr::filter(! keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::bind_rows(indexed_by_contvariable)
  }

  validate_px(x)
}


#' @rdname px_variable_type.px
#' @export
px_variable_type <- function(x, value) {
  UseMethod("px_variable_type")
}

#' @title VARIABLE-TYPE
#'
#' @description `px_description_table1("VARIABLE-TYPE")`
#'
#' @param x A px object
#' @param value A data frame with columns 'variable-code' and 'type'. If value
#' is missing, the current VARIABLE-TYPE is returned. If NULL, all
#' VARIABLE-TYPE is removed.
#'
#' @return A px object or a data frame.
#'
#' @export
px_variable_type.px <- function(x, value) {
  if (missing(value)) {
    return(get_variable1_value(x, "variable-type"))
  } else if (is.null(value)) {
    x$variables1$`variable-type` <- NA
  } else {
    x <- modify_variables1(x, "variable-type", value)
  }

  validate_px(x)
}
