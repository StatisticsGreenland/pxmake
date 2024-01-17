#' Get variable code and label
#'
#' Get a data frame with variable codes and label in all languages.
#' If VARIABLECODE is used, the relation is defined there, but otherwise the
#' variable code is set to be the main languages' label.
#'
#' @inheritParams sort_metadata_df
#'
#' @returns A data frame
get_variable_label <- function(metadata_df) {
  metadata_df <- add_main_language(metadata_df)

  head_stub <-
    metadata_df %>%
    dplyr::filter(keyword %in% c("HEADING", "STUB")) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(`variable-label` = value) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(keyword, language, main_language, `variable-label`, index)

  head_stub_main_language <-
    dplyr::filter(head_stub, main_language) %>%
    dplyr::rename(main_language_label = `variable-label`) %>%
    dplyr::select(keyword, index, main_language_label)

  metadata_df %>%
    dplyr::filter(keyword == "VARIABLECODE") %>%
    tidyr::unnest(value) %>%
    dplyr::select(`variable-code` = value,
                  `variable-label` = variable,
                  language,
                  main_language
    ) %>%
    dplyr::full_join(head_stub,
                     by = c("variable-label", "language", "main_language")
    ) %>%
    dplyr::left_join(head_stub_main_language, by = c("keyword", "index")) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(`variable-code`),
                                           main_language_label,
                                           `variable-code`
    )
    ) %>%
    dplyr::select(-main_language_label)
}

#' Add boolean main_language column
#'
#' @inheritParams sort_metadata_df
#'
#' @returns A data frame
add_main_language <- function(metadata_df) {
  metadata_df %>%
    replace_na_language_with_main_language() %>%
    dplyr::mutate(main_language = language == get_main_language(.))
}

#' Get the main language from metadata
#'
#' @inheritParams sort_metadata_df
#'
#' @returns Character
get_main_language <- function(metadata_df) {
  metadata_df %>%
    dplyr::filter(keyword == "LANGUAGE") %>%
    tidyr::unnest(value) %>%
    dplyr::pull(value)
}

#' Impute missing language
#'
#' Add main language as language if language is missing.
#'
#' @inheritParams sort_metadata_df
#'
#' @returns A data frame
replace_na_language_with_main_language <- function(metadata_df) {
  metadata_df %>%
    dplyr::mutate(language = tidyr::replace_na(language, get_main_language(.)))
}
