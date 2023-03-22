# Various helper functions. See examples in tests/testthat/...

str_quote <- function(str) {
  stringr::str_c('"', str, '"')
}

add_language_to_keyword <- function(keyword, main_language, language) {
  dplyr::if_else(language == main_language | is.na(language),
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
#' Combine two list, by zipping them together in the order \code{v1[1]},
#' \code{v2[1]}, \code{v1[2]}, \code{v2[2]}, \code{v1[3]}, ...
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

#' Split long strings at commas
#'
#' Long strings are split so they are no longer than 256 characters and end at
#' a comma.
#'
#' @param str Character string
#'
#' @returns A character vector
break_long_lines <- function(str, line_limit = 256) {
  if (nchar(str) > line_limit) {
    split <-
      str %>%
      stringr::str_locate_all('","') %>%
      as.data.frame() %>%
      dplyr::filter(start < line_limit) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::pull(start) + 1

    line_start <- stringr::str_sub(str, 1, split)
    line_end   <- stringr::str_sub(str, split+1, -1)

    return(c(line_start, break_long_lines(line_end)))
  } else {
    return(str)
  }
}
