#' Make template
#'
#' Create a minimal metadata template for a data frame.
#' `make_template` will be deprecated in the future when a px object has been
#' implemented (#179).
#'
#' @param data_df A data frame to create metadata for
#' @param languages A vector of languagecodes. The first value is the main
#' language.
#' @param out_path Path to save .xlsx file at. If not specified a temporary file
#' is created.
#' @param figures_variable String. Variable to use as figures variable
#' @param time_variable String. Variable to use as time variable
#'
#' @return A data frame
#' @export
make_template <- function(data_df,
                          languages = c("en"),
                          time_variable = NULL,
                          figures_variable = NULL,
                          out_path = NULL
                          ) {

  print_out_path <- is.null(out_path)
  if (is.null(out_path)) out_path <- temp_xlsx_file()

  main_language <- languages[1]

  metadata_df <-
    get_metadata_template_from_data(data_df = data_df,
                                    heading_variables = time_variable,
                                    figures_variable = figures_variable
                                    ) %>%
    dplyr::mutate(value = ifelse(keyword == "LANGUAGE", main_language, value))

  if(length(languages) > 1) {
    metadata_df <-
      metadata_df %>%
      dplyr::bind_rows(
        dplyr::tribble(~keyword, ~value,
                       "LANGUAGES", languages
                       )
        )

    language_dependent_keywords <-
      get_px_keywords() %>%
      dplyr::filter(language_dependent) %>%
      dplyr::pull(keyword)

    metadata_df <-
      metadata_df %>%
      dplyr::filter(keyword %in% language_dependent_keywords) %>%
      dplyr::select(-language) %>%
      tidyr::expand_grid(language = languages) %>%
      dplyr::bind_rows(dplyr::filter(metadata_df,
                                     ! keyword %in% language_dependent_keywords
                                     )
                       )
  }

  if(!is.null(time_variable)) {
    time_values <-
      data_df %>%
      dplyr::distinct(across(all_of(time_variable))) %>%
      dplyr::pull(1)

    time_var_df <-
      dplyr::tibble(keyword = "TIMEVAL",
                    language = main_language,
                    variable = time_variable,
                    value = format_time_values(time_values)
                    ) %>%
      wrap_varaible_in_list(value)

    metadata_df <- dplyr::bind_rows(metadata_df, time_var_df)
  }

  metamake(list("data" = data_df, "metadata" = metadata_df),
           out_path = out_path, create_data = FALSE
           )

  if (print_out_path) message('Template created at: ', out_path)
}

#' Create a minimal metadata template
#'
#' Stub, heading, and figure variables can be controlled  with arguments. If not
#' provided, one variable is choosen as heading, one as figures, and the
#' remaining as stub.
#'
#' @param data_df A data frame with data to create metadata for
#' @param stub_variables A character vector with stub variables.
#' @param heading_variables A character vector with heading variables.
#' @param figures_variable Name of figure variable,
#'
#' @returns A data frame
get_metadata_template_from_data <- function(data_df,
                                            stub_variables = NULL,
                                            heading_variables = NULL,
                                            figures_variable = NULL
                                            ) {
  variable_names <- names(data_df)

  repeat {
    unallocated_variables <- setdiff(variable_names,
                                     c(stub_variables,
                                       heading_variables,
                                       figures_variable
                                       )
                                     )

    if (length(unallocated_variables) == 0) {
      break
    }

    if (length(figures_variable) < 1) {
      figures_variable <- tail(unallocated_variables, 1)
    } else if (length(heading_variables) < 1) {
      heading_variables <- head(unallocated_variables, 1)
    } else {
      stub_variables <- c(stub_variables, head(head(unallocated_variables, 1)))
    }
  }

  data_df <- format_data_df(data_df, figures_variable)

  values <-
    data_df %>%
    dplyr::select(setdiff(names(.), figures_variable)) %>%
    mutate_all_vars_to_character() %>%
    tidyr::pivot_longer(cols = everything(), names_to = "variable") %>%
    dplyr::arrange_all() %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(value = list(unique(value)))

  get_px_keywords() %>%
    dplyr::filter(mandatory,
                  ! keyword %in% c("DATA", "STUB", "HEADING", "VALUES", "DECIMALS")
                  ) %>%
    dplyr::select(keyword) %>%
    dplyr::bind_rows(dplyr::tibble(keyword = c("NOTE", "ELIMINATION", "DOMAIN"))) %>%
    dplyr::mutate(value = list("")) %>%
    dplyr::bind_rows(
      dplyr::tribble(~keyword,             ~value,
                     "STUB",       stub_variables,
                     "HEADING", heading_variables,
                     "DECIMALS",              "0",
                     "LANGUAGE",             "en"
                     ) %>%
        wrap_varaible_in_list(value),
      dplyr::tibble(keyword = "VALUES", values),
      dplyr::tibble(keyword = "VARIABLECODE",
                    variable = figures_variable,
                    value = list(figures_variable)
                    ),
      dplyr::tibble(keyword = "CODEPAGE", value = list('utf-8'))
      ) %>%
    dplyr::mutate(language = "en", cell = NA_character_) %>%
    dplyr::relocate(value, .after = last_col())
}
