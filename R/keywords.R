mandatory_keywords <- function() {
  px_keywords %>% dplyr::filter(mandatory) %>% dplyr::pull(keyword)
}

keywords_indexed_by_contvariable <- function() {
  px_keywords %>% dplyr::filter(indexed_by_contvariable) %>% dplyr::pull(keyword)
}

language_dependant_keywords <- function() {
  px_keywords %>% dplyr::filter(language_dependent) %>% dplyr::pull(keyword)
}

default_value <- function(keyword) {
  px_keywords %>% dplyr::filter(keyword == !!keyword) %>% dplyr::pull(default_value)
}
