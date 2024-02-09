#' Get names of pivot variables
#'
#' @param x A px object
#' @param pivot A string, either "STUB", "HEADING" or "FIGURES"
#'
#' @return A character vector of variable codes
pivot_variables <- function(x, pivot) {
  x$variables1 %>%
    dplyr::arrange(order, `variable-code`) %>%
    dplyr::filter(toupper(pivot) == !!pivot) %>%
    dplyr::pull(`variable-code`)
}

#' Get name of time variable
#'
#' @param x A px object
#'
#' @return Variable code of time variable
time_variable <- function(x) {
  time_var <-
    x$variables1 %>%
    dplyr::filter(toupper(type) == "TIME") %>%
    dplyr::pull(`variable-code`)
}

stub_variables    <- function(x) pivot_variables(x, "STUB")
heading_variables <- function(x) pivot_variables(x, "HEADING")
figures_variable  <- function(x) pivot_variables(x, "FIGURES")
