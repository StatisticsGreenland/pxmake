#' Add 'total' level to variable
#'
#' Add a new level to a variables which is the sum of all other levels.
#'
#' @inheritParams add_totals_to_df
#' @param var Name of variable to add total level to.
#' @param level_name Total level name.
#'
#' @return A data frame
add_total_level_to_var <- function(df,
                                   var,
                                   level_name = "Total",
                                   sum_var = "value") {

  column_order <- names(df)

  df %>%
    dplyr::group_by(across(-any_of(c(var, sum_var)))) %>%
    dplyr::summarise({{sum_var}} := sum(!!rlang::sym(sum_var), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{var}} := level_name) %>%
    dplyr::bind_rows(df) %>%
    dplyr::relocate(all_of(column_order))
}

#' Add 'total' levels to multiple variables
#'
#' Wrapper around \link{add_total_level_to_var} to add levels to total level to
#' multiple variables.
#'
#' @param df Data frame to add total levels to.
#' @param vars List of variables to add total levels to.
#' @param level_names Names of total levels. Should have length 1 or same length
#' as `vars`.
#' @param sum_var String, name of variable to sum over.
add_totals_to_df <- function(df,
                             vars,
                             level_names = "Total",
                             sum_var = "value") {

  params <- data.frame(vars = vars, level_names = level_names)

  for (i in 1:nrow(params)) {
    df <- add_total_level_to_var(df,
                                 var = params$vars[i],
                                 level_name = params$level_names[i],
                                 sum_var = sum_var
                                 )
    }

  return(df)
}

#' @rdname add_totals.px
#' @export
add_totals <- function(x, variables) {
  UseMethod("add_totals")
}

#' Add 'total' level to variables
#'
#' Add 'total' level to multiple variables. The name of the total level is set
#' as 'elimination' in px$variables2, otherwise 'Total' is used. The value of
#' is the sum of the figures variable. NAs are ignored when summing.
#'
#' @param x A px object
#' @param variables List of variables to add total levels to.
#'
#' @return A px object
#'
#' @export
add_totals.px <- function(x, variables) {
  params <-
    x$variables2 %>%
    dplyr::left_join(dplyr::select(x$codelists2, `variable-code`, code, value),
                     by = c("variable-code", "elimination" = "value"),
                     multiple = "all"
                     ) %>%
    dplyr::filter(`variable-code` %in% vars) %>%
    dplyr::mutate(code = ifelse(is.na(code),
                                elimination,
                                code
                                )
                  ) %>%
    dplyr::distinct(`variable-code`, code)

  figures_variable <-
    x$variables1 %>%
    dplyr::filter(pivot == "FIGURES") %>%
    dplyr::pull(`variable-code`)

  x$data <-
    add_totals_to_df(x$data,
                     vars = params$`variable-code`,
                     level_names = params$code,
                     sum_var = figures_variable
                     )

  return(x)
}
