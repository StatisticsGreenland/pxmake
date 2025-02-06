#' Change pivot variables
#'
#' @param x A px object
#' @param pivot Pivot type (STUB, HEADING, FIGURES)
#' @param value A character vector of variable codes to change to the pivot
#' type
#'
#' @returns A px object
#' @keywords internal
change_pivot_variables <- function(x, value, pivot) {
  old_pivot_variables <- get_pivot_variables(x, pivot)

  new_pivot_variables <- unique(c(value, old_pivot_variables))

  order_df <- dplyr::tibble(`variable-code` = new_pivot_variables,
                            order = 1:length(new_pivot_variables)
                            )

  x$variables1 <-
    modify_or_add_row(df = x$variables1,
                      lookup_column = "variable-code",
                      lookup_column_values = value,
                      modify_column = "pivot",
                      new_value = pivot
                      ) %>%
    dplyr::left_join(order_df, by = "variable-code") %>%
    dplyr::mutate(order = ifelse(toupper(pivot) == !!pivot,
                                 .data$order.y,
                                 .data$order.x
                                 )
                  ) %>%
    dplyr::select(-all_of(c("order.y", "order.x"))) %>%
    dplyr::arrange(desc("pivot"), "order") %>%
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
#' @returns A character vector of variable codes
#' @keywords internal
get_pivot_variables <- function(x, pivot) {
  x$variables1 %>%
    dplyr::filter(toupper(.data$pivot) == !!pivot) %>%
    dplyr::arrange(.data$order, .data$`variable-code`) %>%
    dplyr::pull("variable-code")
}

#' @rdname px_stub.px
#' @export
px_stub <- function(x, value, validate) {
  UseMethod("px_stub")
}

#' @eval add_documentation_head_stub("STUB")
#' @seealso \code{\link{px_heading}} \code{\link{px_figures}}
#' @examples
#' x1 <- px(population_gl)
#' # Print STUB
#' px_stub(x1)
#' # Add 'year' to STUB
#' x2 <- px_stub(x1, 'year')
#' px_stub(x2)
#'
#' # Change order of STUB
#' x3 <- px_stub(x2, c('age', 'gender'))
#' px_stub(x3)
px_stub.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_pivot_variables(x, "STUB"))
  }

  return_px(change_pivot_variables(x, value, "STUB"), validate)
}

#' @rdname px_heading.px
#' @export
px_heading <- function(x, value, validate) {
  UseMethod("px_heading")
}

#' @eval add_documentation_head_stub("HEADING")
#' @seealso \code{\link{px_stub}} \code{\link{px_figures}}
#' @examples
#' x1 <- px(population_gl)
#'
#' # Print HEADING
#' px_heading(x1)
#'
#' # Add 'gender' to HEADING
#' x2 <- px_heading(x1, 'gender')
#' px_heading(x2)
#'
#' # Change order of HEADING
#' x3 <- px_heading(x2, 'year')
#' px_heading(x3)
px_heading.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_pivot_variables(x, "HEADING"))
  }

  return_px(change_pivot_variables(x, value, "HEADING"), validate)
}

#' @rdname px_figures.px
#' @export
px_figures <- function(x, value, validate) {
  UseMethod("px_figures")
}

#' @title Change figures variable
#' @description
#' Inspect or change which variable is used as figures. The previous figures
#' variable is changed to STUB. There can only be one figures variable.
#'
#' @param x A px object
#' @param value Optional. Name of variable to use as FIGRUES. If missing, the
#' current PX_FIGURES variable is returned.
#' @eval param_validate()
#'
#' @returns A px object or a character string
#'
#' @seealso \code{\link{px_stub}} \code{\link{px_heading}}
#'
#' @examples
#' x1 <- px(population_gl)
#'
#' # Print FIGURES
#' px_figures(x1)
#'
#' # Change 'age' to FIGURES variable, 'n' i changed to STUB
#' x2 <- px_figures(x1, 'age')
#' px_figures(x2)
#' px_stub(x2)
#' @export
px_figures.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_pivot_variables(x, "FIGURES"))
  }

  error_if_not_exactly_one_figures_variable(value)

  old_figures_variable <- px_figures(x)

  x <- change_pivot_variables(x, value, "FIGURES")

  x <- change_pivot_variables(x, old_figures_variable, "STUB")

  x$cells1 <-
    x$cells1 %>%
    dplyr::filter(!.data$`variable-code` %in% !!value)

  x$cells2 <-
    x$cells2 %>%
    dplyr::filter(!.data$`variable-code` %in% !!value)

  return_px(x, validate)
}

#' @rdname px_timeval.px
#' @export
px_timeval <- function(x, value, validate) {
  UseMethod("px_timeval")
}

#' @eval add_documentation_table1("TIMEVAL", "year")
#' @description There can only be one time variable.
px_timeval.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_variable1_logic_value(x, "timeval"))
  } else if (is.null(value)) {
    x$variables1$timeval <- FALSE
  } else {
    x$variables1$timeval <- FALSE

    x <- modify_variables1(x, "timeval",
                           dplyr::tibble(`variable-code` = value,
                                         timeval = TRUE
                                         )
                           )

    # TIMEVAL variables should not be in cells1/2
    x$cells1  <-
      x$cells1 %>%
      dplyr::filter(!.data$`variable-code` %in% !!rlang::syms(value))

    x$cells2  <-
      x$cells2 %>%
      dplyr::filter(!.data$`variable-code` %in% !!rlang::syms(value))
  }

  return_px(x, validate)
}

#' @rdname px_contvariable.px
#' @export
px_contvariable <- function(x, value, validate) {
  UseMethod("px_contvariable")
}

#' @eval add_doc_keyword_function_intro("CONTVARIABLE")
#' @description Setting CONTVARIABLE indexes several variables in table2.
#' Removing CONTVARIABLE removes the indexing from table2.
#' @param value `r table1_param_value("CONTVARIABLE")`
#' @eval param_validate()
#' @eval add_return_px_or_char_str()
#' @examples
#' # Set CONTVARIABLE
#' x1 <-
#'   px(population_gl) |>
#'   px_contvariable('gender')
#'
#' # After setting CONTVARIABLE some variables are index by it, e.g. UNITS
#' px_units(x1)
#'
#' # Remove CONTVARIABLE
#' x2 <- px_contvariable(x1, NULL)
#' px_contvariable(x2)
#'
#' # Removing CONTVARIABLE also removes the index from UNITS
#' px_units(x2)
#' @export
px_contvariable.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_variable1_logic_value(x, "contvariable"))
  } else if (is.null(value)) {
    x$variables1$contvariable <- FALSE

    previously_indexed_by_contvariable <-
     x$table2 %>%
     dplyr::filter(.data$keyword %in% keywords_indexed_by_contvariable()) %>%
     dplyr::group_by(.data$keyword, .data$language) %>%
     dplyr::slice(1)

    x$table2 <-
      x$table2 %>%
      dplyr::filter(! .data$keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::bind_rows(previously_indexed_by_contvariable) %>%
      dplyr::mutate(code = NA_character_)
  } else {
    x$variables1$contvariable <- FALSE

    x <- modify_variables1(x, "contvariable",
                           dplyr::tibble("variable-code" = value,
                                         "contvariable" = TRUE
                                         )
                           )


    contvariable_codes <- unique(x$data[[value]]) %>% as.character()

    indexed_by_contvariable <-
      x$table2 %>%
      dplyr::filter(.data$keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::select(-"code") %>%
      tidyr::crossing(code = contvariable_codes)

    x$table2 <-
      x$table2 %>%
      dplyr::filter(! .data$keyword %in% keywords_indexed_by_contvariable()) %>%
      dplyr::bind_rows(indexed_by_contvariable)
  }

  return_px(x, validate)
}


#' @rdname px_variable_type.px
#' @export
px_variable_type <- function(x, value, validate) {
  UseMethod("px_variable_type")
}

#' @eval add_doc_keyword_function_intro("VARIABLE-TYPE")
#' @param value A data frame with columns 'variable-code' and 'variable-type'.
#' If value is missing, the current VARIABLE-TYPE is returned. If NULL, all
#' VARIABLE-TYPE is removed.
#' @eval param_validate()
#' @eval add_return_px_or_df()
#' @examples
#' library(tibble)
#'
#' # Set VARIABLE-TYPE
#' x1 <-
#'   px(population_gl) |>
#'   px_variable_type(tibble('variable-code' = 'year', 'variable-type' = 'time'))
#'
#' # Print VARIABLE-TYPE
#' px_variable_type(x1)
#'
#' # Remove VARIABLE-TYPE
#' x2 <- px_variable_type(x1, NULL)
#' px_variable_type(x2)
#'
#' @export
px_variable_type.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(get_variable1_value(x, "variable-type"))
  } else if (is.null(value)) {
    x$variables1$`variable-type` <- NA
  } else {
    x <- modify_variables1(x, "variable-type", value)
  }

  return_px(x, validate)
}
