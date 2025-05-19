#' Add total level to variable
#'
#' Add a new level to a variables which is the sum of all other levels.
#'
#' @inheritParams add_totals_to_df
#' @param variable Name of variable to add total level to.
#' @param level_name Total level name.
#'
#' @returns A data frame
#' @keywords internal
add_total_level_to_var <- function(df,
                                   variable,
                                   level_name = "Total",
                                   sum_var = "value",
                                   na.rm = TRUE
                                   ) {

  column_order <- names(df)

  df %>%
    dplyr::group_by(across(-any_of(c(variable, sum_var)))) %>%
    dplyr::summarise(!!sum_var := sum(!!rlang::sym(sum_var), na.rm = !!na.rm)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{variable}} := level_name) %>%
    dplyr::bind_rows(df) %>%
    dplyr::relocate(all_of(column_order))
}

#' Add total levels to multiple variables
#'
#' Wrapper around \link{add_total_level_to_var} to add levels to total level to
#' multiple variables.
#'
#' @inheritParams px_add_totals
#' @param df Data frame to add total levels to.
#' @param variables List of variables to add total levels to.
#' @param level_names Names of total levels. Should have length 1 or same length
#' as `variables`.
#' @param sum_var String, name of variable to sum over.
#' @keywords internal
add_totals_to_df <- function(df,
                             variables,
                             level_names,
                             sum_var = "value",
                             na.rm = TRUE
                             ) {
  params <- data.frame(variables = variables, level_names = level_names)

  for (i in 1:nrow(params)) {
    df <- add_total_level_to_var(df,
                                 variable = params$variables[i],
                                 level_name = params$level_names[i],
                                 sum_var = sum_var,
                                 na.rm = !!na.rm
                                 )
    }

  return(df)
}

#' @rdname px_add_totals.px
#' @export
px_add_totals <- function(x, value, na.rm = TRUE, validate) {
  UseMethod("px_add_totals")
}

#' @title Add total levels to variables
#'
#' @description
#' Adds a total level, which is the sum of the figures for all other levels of
#' the variable.
#'
#' The default name of the total level is 'Total', unless \link{px_elimination}
#' is set, in which case the elimination code is used.
#'
#' @param x A px object
#' @param value A character vector of variables to add total levels to.
#' @param na.rm Optional. Logical. If TRUE, NAs are removed before summing.
#' @eval param_validate()
#'
#' @returns A px object
#'
#' @seealso [px_elimination]
#'
#' @examples
#' # Create small px object example
#' x0 <- px(subset(population_gl, age == "65+"))
#' px_data(x0)
#'
#' # Add total level to one variable
#' x1 <- px_add_totals(x0, "gender")
#' px_data(x1)
#'
#' # Add total level to multiple variables
#' x2 <- px_add_totals(x0, c("gender", "age"))
#' px_data(x2)
#'
#' # The name of the total level can be changed with px_elimination()
#' x3 <-
#'   x0 |>
#'   px_elimination("T") |>
#'   px_add_totals("gender")
#'
#' px_data(x3)
#'
#' @export
px_add_totals.px <- function(x, value, na.rm = TRUE, validate = TRUE) {
  elimination <-
    px_elimination(x)

  values_not_in_elimination <-
    value[!value %in% elimination$`variable-code`]

  if (length(values_not_in_elimination) > 0) {
    default_elimination <-
      dplyr::tibble(`variable-code` = values_not_in_elimination,
                    elimination = "Total"
                    )

    x <- px_elimination(x, dplyr::bind_rows(elimination, default_elimination))
  }

  params <-
    x$variables1 %>%
    dplyr::filter(.data$`variable-code` %in% value) %>%
    dplyr::distinct(.data$`variable-code`, .data$elimination)

  x$data <-
    add_totals_to_df(x$data,
                     variables = params$`variable-code`,
                     level_names = params$elimination,
                     sum_var = px_figures(x),
                     na.rm = !!na.rm
                     )

  return_px(x, validate)
}
