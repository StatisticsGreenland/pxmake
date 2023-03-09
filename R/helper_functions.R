# Various helper functions. See examples in tests/testthat/...

str_quote <- function(str) {
  stringr::str_c('"', str, '"')
}

add_language_to_keyword <- function(keyword, language) {
  dplyr::if_else(language == "en" | is.na(language),
                 keyword,
                 stringr::str_glue("{keyword}[{language}]") %>% as.character()
                 )
}

add_sub_key_to_keyword <- function(keyword, name) {
  stringr::str_glue('{keyword}("{name}")')
}

quote_unless_numeric_or_yes_no <- function(str) {
  str_is_numeric <- function(str) {
    stringr::str_detect(str, "^[0-9.]+$")
  }

  str_is_quoted <- function(str) {
    stringr::str_length(str) >= 2 &
      stringr::str_sub(str, 1, 1) == '"' &
      stringr::str_sub(str, -1, -1) == '"'
  }

  dplyr::if_else(
    str %in% c('YES', 'NO') | str_is_numeric(str) | str_is_quoted(str) |
      stringr::str_starts(str, "TLIST\\("),
    str,
    stringr::str_c('"', str, '"')
  )
}

lst_distinct_and_arrange <- function(lst) {
  tmp <- lapply(lapply(lst, unique), sort)
  tmp[order(names(tmp))]
}

merge_named_lists <- function(lst1, lst2) {
  keys <- unique(c(names(lst1), names(lst2)))
  temp <- setNames(mapply(c, lst1[keys], lst2[keys]), keys)
  lst_distinct_and_arrange(temp)
}

get_timeval_type_from_values <- function(values) {
  time_type <-
    values %>%
    na.omit() %>%
    stringr::str_replace_all('[:digit:]', '') %>%
    paste(collapse = '') %>%
    stringr::str_sub(1, 1)

  if (time_type == '') {
    time_type <- 'A'
  }

  time_type
}

#' Zips list
#'
#' Combine two list, by zipping them togehter in the order v1[1], v2[1], v1[2],
#' v2[2], v1[3], ...
#'
#' @param v1 First vector Elements ends up on uneven indexes.
#' @param v2 Second vector. Elements ends up on even indexes.
#'
#' @returns List
zip_vectors <- function(v1, v2) {
  if (length(v1) != length(v2)) {
    stop("v1 and v2 must have same length.")
  }

  matrix(c(v1, v2), ncol = 2) %>% t() %>% as.list() %>% unlist()
}
