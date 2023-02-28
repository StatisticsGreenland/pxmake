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
    str %in% c('YES', 'NO') | str_is_numeric(str) | str_is_quoted(str),
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

