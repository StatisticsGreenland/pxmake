#' Make template
#'
#' Create a minimal metadata template for a data frame. The last column, becomes
#' the FIGURES column. `make_template` will be deprecated in the future when a
#' px object has been implemented (#179).
#'
#' @param data_df A data frame to create metadata for
#' @param languages A vector of languagecodes. The first value is the main
#' language.
#' @param out_path Path to save .xlsx file at. If not specified a temporary file
#' is created.
#' @param time_variable String. Variable to use as time variable
#'
#' @return A data frame
#' @export
make_template <- function(data_df,
                          languages = c("en"),
                          time_variable = NULL,
                          out_path = NULL
                          ) {

  if (is.null(out_path)) out_path <- temp_xlsx_file()

  main_language <- languages[1]

  metadata_df <-
    get_metadata_template_from_data(data_df) %>%
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
                    value = paste0("TLIST(",
                                   get_timeval_type_from_values(time_values),
                                   "1),",
                                   time_values %>%
                                     stringr::str_replace_all('[:alpha:]', '') %>%
                                     str_quote() %>%
                                     stringr::str_c(collapse = ',')
                    )
      ) %>%
      wrap_varaible_in_list(value)

    metadata_df <- dplyr::bind_rows(metadata_df, time_var_df)
  }

  metamake(list("data" = data_df, "metadata" = metadata_df),
           out_path = out_path
           )

  message('Template created at: ', out_path)
}

