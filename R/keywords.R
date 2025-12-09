mandatory_keywords <- function() {
  pxmake::px_keywords %>%
    dplyr::filter(.data$mandatory) %>%
    dplyr::pull("keyword")
}

keywords_indexed_by_contvariable <- function() {
  pxmake::px_keywords %>%
    dplyr::filter(.data$indexed_by_contvariable) %>%
    dplyr::pull("keyword")
}

language_dependant_keywords <- function() {
  pxmake::px_keywords %>%
    dplyr::filter(.data$language_dependent) %>%
    dplyr::pull("keyword")
}

default_value <- function(keyword) {
  pxmake::px_keywords %>%
    dplyr::filter(.data$keyword == !!keyword) %>%
    dplyr::pull("default_value")
}
