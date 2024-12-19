mandatory_keywords <- function() {
  px_keywords %>% dplyr::filter(.data$mandatory) %>% dplyr::pull("keyword")
}

keywords_indexed_by_contvariable <- function() {
  px_keywords %>% dplyr::filter(.data$indexed_by_contvariable) %>% dplyr::pull("keyword")
}

language_dependant_keywords <- function() {
  px_keywords %>% dplyr::filter(.data$language_dependent) %>% dplyr::pull("keyword")
}

default_value <- function(keyword) {
  px_keywords %>%
    dplyr::filter(.data$keyword == !!keyword) %>%
    dplyr::pull("default_value")
}
