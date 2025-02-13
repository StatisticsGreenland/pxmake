#' Format df for px format
#'
#' Turn all variables, except figures variable, into character and replace NA
#' with dash.
#'
#' @param data_df A data frame with data.
#' @param figures_variable Character. The name of the figures variable.
#'
#' @returns A data frame
#' @keywords internal
format_data_df <- function(data_df, figures_variable) {
  data_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(-one_of(intersect(names(.), figures_variable)),
                         ~factor(.x, ordered = FALSE)
                         )
                  ) %>%
    dplyr::mutate(dplyr::across(where(~ is.factor(.x) && anyNA(.x)),
                                ~ forcats::fct_na_value_to_level(.x, level = "-")
                                )
                  )
}

#' Create a minimal px object from a data frame
#'
#' @param df A data frame
#'
#' @returns A px object
#' @keywords internal
px_from_data_df <- function(df) {
  default_language <- NA

  mandatory_table_keywords <-
    px_keywords %>%
    dplyr::filter(.data$mandatory, .data$table_meta)

  table1 <-
    mandatory_table_keywords %>%
    dplyr::filter(!.data$language_dependent) %>%
    dplyr::select("keyword", "value" = "default_value") %>%
    align_data_frames(get_base_table1()) %>%
    sort_table1()

  table2 <-
    mandatory_table_keywords %>%
    dplyr::filter(.data$language_dependent) %>%
    dplyr::select("keyword", "value" = "default_value") %>%
    dplyr::mutate(language = default_language) %>%
    align_data_frames(get_base_table2()) %>%
    sort_table2()

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
    tidyr::unnest("variable-code") %>%
    dplyr::group_by(.data$pivot) %>%
    dplyr::mutate(order = ifelse(.data$pivot == "FIGURES",
                                 NA,
                                 dplyr::row_number()
                                 ),
                  contvariable = FALSE
                  ) %>%
    dplyr::ungroup() %>%
    align_data_frames(get_base_variables1()) %>%
    sort_variables1()

  variables2 <-
    variables1 %>%
    dplyr::select("variable-code") %>%
    dplyr::mutate(language = default_language,
                  `variable-label` = .data$`variable-code`
                  ) %>%
    align_data_frames(get_base_variables2()) %>%
    sort_variables2(data_table_names = names(df),
                    languages = default_language
                    )

  if (length(df) == 0) {
    cells1 <- get_base_cells1()
  } else {
    cells1 <-
      data_df %>%
      dplyr::select(all_of(setdiff(names(.), figures_variable))) %>%
      tidyr::pivot_longer(cols = everything(),
                          cols_vary = "slowest",
                          names_to = "variable-code",
                          values_to = "code"
                          ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$`variable-code`) %>%
      dplyr::mutate(order = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      align_data_frames(get_base_cells1()) %>%
      sort_cells1(data_table_names = names(data_df))
  }

  cells2 <-
    cells1 %>%
    dplyr::select("variable-code", "code") %>%
    dplyr::mutate(language = default_language,
                  value = .data$code
                  ) %>%
    align_data_frames(get_base_cells2()) %>%
    sort_cells2(data_table_names = names(data_df),
                languages = default_language
                )

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
