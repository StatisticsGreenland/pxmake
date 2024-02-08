pivot_variables <- function(px, pivot) {
  px$variables1 %>%
    dplyr::arrange(order, `variable-code`) %>%
    dplyr::filter(toupper(pivot) == !!pivot) %>%
    dplyr::pull(`variable-code`)
}

time_variable <- function(px) {
  time_var <-
    px$variables1 %>%
    dplyr::filter(toupper(type) == "TIME") %>%
    dplyr::pull(`variable-code`)
}

stub_variables    <- function(px) pivot_variables(px, "STUB")
heading_variables <- function(px) pivot_variables(px, "HEADING")
figures_variable  <- function(px) pivot_variables(px, "FIGURES")


