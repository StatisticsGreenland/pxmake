#' Format df for px format
#'
#' Turn all variables, except figures variable, into character and replace NA
#' with dash.
#'
#' @param data_df A data frame with data.
#' @param figures_variable Character. The name of the figures variable.
#'
#' @return A data frame
#' @keywords internal
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

#' Create a minimal px object from a data frame
#'
#' @param df A data frame
#'
#' @return A px object
#' @keywords internal
px_from_data_df <- function(df) {
  default_language <- NA

  mandatory_table_keywords <-
    px_keywords %>%
    dplyr::filter(mandatory, table_meta)

  table1 <-
    mandatory_table_keywords %>%
    dplyr::filter(!language_dependent) %>%
    dplyr::select(keyword, value = default_value) %>%
    align_data_frames(get_base_table1())

  table2 <-
    mandatory_table_keywords %>%
    dplyr::filter(language_dependent) %>%
    dplyr::select(keyword, value = default_value) %>%
    dplyr::mutate(language = default_language) %>%
    align_data_frames(get_base_table2())

  variable_names <- names(df)

  stub_variables    <- c()
  heading_variables <- c()
  figures_variable  <- c()

  figures_variable <- tail(variable_names, 1)

  data_df <- format_data_df(df, figures_variable)

  if (length(variable_names) >= 3) {
    heading_variables <- head(tail(variable_names, 2), 1)
    stub_variables    <- head(variable_names, length(variable_names) - 2)
  } else {
    heading_variables <-  NULL
    stub_variables    <- head(variable_names, length(variable_names) - 1)
  }

  variables1 <-
    dplyr::tribble(~`variable-code`, ~pivot,
                   stub_variables,    "STUB",
                   heading_variables, "HEADING",
                   figures_variable,  "FIGURES"
                   ) %>%
    tidyr::unnest(`variable-code`) %>%
    dplyr::group_by(pivot) %>%
    dplyr::mutate(order = ifelse(pivot == "FIGURES", NA, dplyr::row_number()),
                  contvariable = FALSE
                  ) %>%
    dplyr::ungroup() %>%
    align_data_frames(get_base_variables1())

  variables2 <-
    variables1 %>%
    dplyr::select(`variable-code`) %>%
    dplyr::mutate(language = default_language,
                  `variable-label` = `variable-code`
                  ) %>%
    align_data_frames(get_base_variables2())

  if (length(df) == 0) {
    cells1 <- get_base_cells1()
  } else {
    cells1 <-
      data_df %>%
      dplyr::select(setdiff(names(.), figures_variable)) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "variable-code",
                          values_to = "code"
                          ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(`variable-code`, code) %>%
      dplyr::group_by(`variable-code`) %>%
      dplyr::mutate(order = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      align_data_frames(get_base_cells1())
  }

  cells2 <-
    cells1 %>%
    dplyr::select(`variable-code`, code) %>%
    dplyr::mutate(language = default_language,
                  value = code
                  ) %>%
    align_data_frames(get_base_cells2())

  new_px(languages = get_base_languages(),
         table1 = table1,
         table2 = table2,
         variables1 = variables1,
         variables2 = variables2,
         cells1 = cells1,
         cells2 = cells2,
         acrosscells = get_base_acrosscells(c(stub_variables, heading_variables)),
         data = data_df
         ) %>%
    px_title("") %>%
    px_charset('ANSI')
}
