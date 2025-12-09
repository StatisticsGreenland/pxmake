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
                `variable-type` = as.character(),
                contvariable    = as.logical(),
                timeval         = as.logical(),
                elimination      = as.character()
                )
}

get_base_variables2 <- function() {
  dplyr::tibble(`variable-code`  = as.character(), #key
                language         = as.character(), #key
                `variable-label` = as.character(),
                domain           = as.character(),
                map              = as.character(),
                note             = as.character(),
                notex            = as.character()
                )
}

get_base_cells1 <- function() {
  dplyr::tibble(`variable-code` = as.character(), #key
                code            = as.character(), #key
                order           = as.numeric(),
                precision       = as.numeric()
                )
}

get_base_cells2 <- function() {
  dplyr::tibble(`variable-code` = as.character(), #key
                code            = as.character(), #key
                language        = as.character(), #key
                value           = as.character(),
                valuenote       = as.character(),
                valuenotex      = as.character()
                )
}

get_base_acrosscells <- function(stub_heading_variables = NULL) {
  data_columns <- character0_tibble(stub_heading_variables)

  dplyr::bind_cols(data_columns,
                   dplyr::tibble(language  = as.character(),
                                 cellnote  = as.character(),
                                 cellnotex = as.character()
                                 )
                   )
}

get_base_data <- function() {
  dplyr::tibble()
}

get_base_px <- function() {
  structure(list(languages = get_base_languages(),
                 table1 = get_base_table1(),
                 table2 = get_base_table2(),
                 variables1 = get_base_variables1(),
                 variables2 = get_base_variables2(),
                 cells1 = get_base_cells1(),
                 cells2 = get_base_cells2(),
                 acrosscells = get_base_acrosscells(),
                 data = get_base_data()
                 ),
            class = "px"
            )
}

sort_in_keyword_order <- function(df) {
  df %>%
    dplyr::left_join(dplyr::select(pxmake::px_keywords, "keyword", "order"),
                     by = "keyword"
                     ) %>%
    dplyr::arrange(.data$order) %>%
    dplyr::select(-"order")
}

sort_table1 <- sort_in_keyword_order

sort_table2 <- function(df, languages) {
  df %>%
    dplyr::left_join(dplyr::select(pxmake::px_keywords, "keyword", "order"),
                     by = "keyword"
                     ) %>%
    dplyr::arrange(.data$order, !is.na(.data$code), .data$code,
                   match(.data$language, languages)
                   ) %>%
    dplyr::select(-"order")
}

sort_variables1 <- function(df) {
  df %>%
    dplyr::arrange(desc(.data$pivot), .data$order)
}

sort_variables2 <- function(df, data_table_names, languages) {
  df %>%
    dplyr::arrange(match(.data$`variable-code`, data_table_names),
                   match(.data$language, languages)
                   )
}

sort_cells1 <- function(df, data_table_names) {
  df %>%
    dplyr::arrange(match(.data$`variable-code`, data_table_names),
                   .data$order
                   )
}

sort_cells2 <- function(df, data_table_names, languages) {
  df %>%
    dplyr::arrange(match(.data$`variable-code`, data_table_names),
                   .data$code,
                   match(.data$language, languages)
                   )
}
