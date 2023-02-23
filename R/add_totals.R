#' Add 'total' level to variable
#'
#' @param df Data frame
#' @param var String, name of variable to add a total level in
#' @param level_name Total level name
#' @param sum_var String, name of variable to sum over
#'
#' @return Data frame
add_total_level_to_var <- function(df,
                                   var,
                                   level_name = "Total",
                                   sum_var = "value") {

  column_order <- names(df)

  df %>%
    dplyr::group_by(across(-any_of(c(var, sum_var)))) %>%
    dplyr::summarise({{sum_var}} := sum(!!rlang::sym(sum_var))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{var}} := level_name) %>%
    dplyr::bind_rows(df) %>%
    dplyr::relocate(column_order)
}

#' Add 'total' level to variables
#'
#' @param df Data frame
#' @param vars List of variables to add total levels to
#' @param level_names Names of total levels. Should have length 1 or same length
#' as `vars`.
#' @param sum_var String, name of variable to sum over
add_totals <- function(df,
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
