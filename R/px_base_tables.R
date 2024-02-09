# Define base structures for px object
get_base_languages <- function() {
  dplyr::tibble(language = as.character())
}

get_base_table1 <- function() {
  dplyr::tibble(keyword = as.character(),
                value   = as.character()
                )
}

get_base_table2 <- function() {
  dplyr::tibble(keyword  = as.character(),
                code     = as.character(),
                language = as.character(),
                value    = as.character()
                )
}

get_base_variables1 <- function() {
  dplyr::tibble(`variable-code` = as.character(), #key
                pivot           = as.character(),
                order           = as.numeric(),
                type            = as.character()
                )
}

get_base_variables2 <- function() {
  dplyr::tibble(`variable-code`  = as.character(), #key
                language         = as.character(),
                `variable-label` = as.character(),
                domain           = as.character(),
                elimination      = as.character(),
                note             = as.character()
                )
}

get_base_codelists1 <- function() {
  dplyr::tibble(`variable-code` = as.character(), #key
                code            = as.character(), #key
                order           = as.numeric(),
                precision       = as.numeric()
                )
}

get_base_codelists2 <- function() {
  dplyr::tibble(`variable-code` = as.character(), #key
                code            = as.character(), #key
                language        = as.character(),
                value           = as.character(),
                `valuenote`     = as.character()
                )
}

get_base_data <- function() {
  dplyr::tibble()
}

get_base_px <- function() {
  list(languages = get_base_languages(),
       table1 = get_base_table1(),
       table2 = get_base_table2(),
       variables1 = get_base_variables1(),
       variables2 = get_base_variables2(),
       codelists1 = get_base_codelists1(),
       codelists2 = get_base_codelists2(),
       data = get_base_data()
       )
}
