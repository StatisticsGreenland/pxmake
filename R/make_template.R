#' Make template
#'
#' Create a minimal metadata template for a data frame. This function will be
#' deprecated in the future when px object has been implemented (#179).
#'
#' @param data_df A data frame to create metadata for
#' @param languages A vector of languagecodes
#' @param out_path Path to save .xlsx file at
#'
#' @return A data frame
#' @export
make_template <- function(data_df, languages = c("en"), out_path = temp_xlsx_file()) {
  metadata_df <-
    get_metadata_template_from_data(data_df) %>%
    dplyr::mutate(value = ifelse(keyword == "LANGUAGE", languages[1], value))

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

  metamake(list("data" = data_df, "metadata" = metadata_df),
           out_path = out_path
           )

  message('Template created at: ', out_path)
}
