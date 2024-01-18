#' Format df for px format
#'
#' Turn all variables, except figures variable, into character and replace NA
#' with dash.
#'
#' @param data_df A data frame with data.
#' @param figures_variable Character. The name of the figures variable.
#'
#' @returns A data frame
format_data_df <- function(data_df, figures_variable) {
  data_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(-one_of(intersect(names(.), figures_variable)),
                         as.character
                         )
                  ) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ tidyr::replace_na(.x, "-")
                                )
                  )
}
