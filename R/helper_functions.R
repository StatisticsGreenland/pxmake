# Various helper functions. See test cases in test-00-helper_function.R for
# examples of function input and outputs.

#' Add quotes around string
#'
#' @param str String to quote
#'
#' @returns String
#' @keywords internal
str_quote <- function(str) {
  stringr::str_c('"', str, '"')
}

#' Add language, other than the main langue, to keyword
#'
#' @inheritParams add_sub_key_to_keyword
#' @param main_language Main language of PX-file
#' @param language Lange to add to keyword
#'
#' @returns String
#' @keywords internal
add_language_to_keyword <- function(keyword, main_language, language) {
  ifelse(language == main_language | is.na(language),
         keyword,
         stringr::str_glue("{keyword}[{language}]") %>% as.character()
         )
}

#' Add a sub key word (see px-specification for details on sub keys)
#'
#' @param keyword String, name of keyword
#' @param name String, name of sub key
#'
#' @returns String
#' @keywords internal
add_sub_key_to_keyword <- function(keyword, name) {
  ifelse(is.na(name),
         keyword,
         stringr::str_glue('{keyword}("{name}")')
         )
}

#' Add cell name to keywords that support it (see px-specification)
#'
#' @inheritParams add_sub_key_to_keyword
#' @param name String, cell name to add to keyword
#'
#' @returns String
#' @keywords internal
add_cell_to_keyword <- function(keyword, name) {
  keyword_has_subkey <- stringr::str_sub(keyword, -1) == ")"

  ifelse(is.na(name),
         keyword,
         ifelse(keyword_has_subkey,
                stringr::str_glue('{stringr::str_sub(keyword, 1, -2)},"{name}")'),
                add_sub_key_to_keyword(keyword, name)
                )
         )
}

#' Add quotes around unless in some very specific cases required by the px format
#'
#' @inheritParams str_quote
#'
#' @returns String
#' @keywords internal
quote_unless_yes_no <- function(str) {
  str_is_quoted <- function(str) {
    stringr::str_length(str) >= 2 &
      stringr::str_sub(str, 1, 1) == '"' &
      stringr::str_sub(str, -1, -1) == '"'
  }

  ifelse(
    str %in% c('YES', 'NO') | str_is_quoted(str) |
      stringr::str_starts(str, "TLIST\\("),
    str,
    str_quote(str)
  )
}

#' Get a sorted list of distinct values in list
#'
#' @param lst List to sort
#'
#' @returns List
#' @keywords internal
lst_distinct_and_arrange <- function(lst) {
  if (length(lst) == 0) {
    return(lst)
  }

  tmp <- lapply(lapply(lst, unique), sort)
  tmp[order(names(tmp))]
}

#' Put two named lists together, remove duplicates and sort
#'
#' @param lst1 List to merge
#' @param lst2 List to merge
#'
#' @returns List
#' @keywords internal
merge_named_lists <- function(lst1, lst2) {
  if (length(lst1) == 0) {
    lst1 <- lst2
  } else if (length(lst2) == 0) {
    lst2 <- lst1
  }

  keys <- sort(unique(c(names(lst1), names(lst2))))

  add_missing_keys <- function(lst, keys) {
    keys_missing_in_list <- setdiff(keys, names(lst))
    lst[keys_missing_in_list] <- NA_character_
    return(lst)
  }

  lst1_sorted <-
    lst1 %>%
    add_missing_keys(keys) %>%
    lst_distinct_and_arrange()

  lst2_sorted <-
    lst2 %>%
    add_missing_keys(keys) %>%
    lst_distinct_and_arrange()

  if (identical(lst1_sorted, lst2_sorted)) {
    temp <- lst1_sorted
  } else {
    temp <- setNames(mapply(c, lst1_sorted, lst2_sorted, SIMPLIFY = FALSE), keys)
  }

  lst_distinct_and_arrange(temp)
}

#' Get time scale code from values (see TIMEVAL in px-specification)
#'
#' @param values Values form PX-file
#'
#' @returns A character vector
#' @keywords internal
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

  return(time_type)
}

#' Format time values for PX-file
#'
#' @param values Time values
#'
#' @returns A character vector
#' @keywords internal
format_time_values <- function(values) {
  paste0("TLIST(",
         get_timeval_type_from_values(values),
         "1),",
         values %>%
           stringr::str_replace_all('[:alpha:]', '') %>%
           str_quote() %>%
           stringr::str_c(collapse = ',')
         )
}

#' Get time values from TIMEVAL string
#'
#' @param str String with time values
#'
#' @returns A character vector
#' @keywords internal
get_values_from_time_format <- function(str) {
  short_syntax = any(stringr::str_detect(str, "-"))

  tmp <-
    str %>%
    stringr::str_split(',') %>%
    unlist()

  tlist      <- tmp %>% head(1)
  type       <- stringr::str_sub(tlist, 7, 7)
  value_part <- tmp %>% tail(-1)

  if (short_syntax) {
    times <-
      value_part %>%
      stringr::str_replace_all("[^0-9-]", "") %>%
      stringr::str_split("-", simplify = TRUE)

    interval_start <- times[1]
    interval_end   <- times[2]

    if (type == "A")
      values <- as.character(seq(interval_start, interval_end))
    else {
      indicies_per_year <- c(H = 2, Q = 4, M = 12)

      year_start <- stringr::str_sub(interval_start, 1, 4)
      year_end   <- stringr::str_sub(interval_end,   1, 4)

      values <-
        tidyr::crossing(year = seq(year_start, year_end),
                        index = seq(1, indicies_per_year[type])
                        ) %>%
        dplyr::mutate(value = paste0(.data$year, .data$index), .keep="none") %>%
        dplyr::filter(as.numeric(.data$value) < interval_end) %>%
        dplyr::pull(1)
    }
  } else {
    values <-
      value_part %>%
      stringr::str_replace_all('"', '')
  }

  if (type == "A") {
    return(values)
  } else {
    return(paste0(stringr::str_sub(values, 1, 4), type, stringr::str_sub(values, 5)))
  }
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
#' @keywords internal
zip_vectors <- function(v1, v2) {
  if (length(v1) != length(v2)) {
    stop("v1 and v2 must have same length.")
  }

  base::matrix(c(v1, v2), ncol = 2) %>% t() %>% as.list() %>% unlist()
}

#' Split long strings at commas
#'
#' Long strings are split so they are no longer than 256 characters and end at
#' a comma.
#'
#' @param str String
#' @param max_line_length Integer longest allowed line length
#'
#' @returns A character vector
#' @keywords internal
break_long_lines <- function(str, max_line_length = 256) {
  if (is.null(str)) {
    return("")
  } else if (is.na(str)) {
    return("")
  } else if (nchar(str) > max_line_length) {
    comma_split <-
      str %>%
      stringr::str_locate_all('","') %>%
      as.data.frame() %>%
      dplyr::filter(.data$start < max_line_length) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::pull(.data$start)

    if (identical(comma_split, integer(0))) {
      # no comma_split character; split at specific point
      line_start <- paste0(stringr::str_sub(str, 1, max_line_length - 2), '"')
      line_end   <- paste0('"', stringr::str_sub(str, max_line_length - 1))
    } else {
      line_start <- stringr::str_sub(str, 1, comma_split + 1)
      line_end   <- stringr::str_sub(str, comma_split + 2, -1)
    }

    return(c(line_start, break_long_lines(line_end, max_line_length)))
  } else {
    return(str)
  }
}

#' Convert a variable to a list
#'
#' @param df Data frame
#' @param var Character. Variable to convert to list
#'
#' @returns A data frame
#' @keywords internal
wrap_varaible_in_list <- function(df, var) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(across(all_of(var), ~ list(.x))) %>%
    dplyr::ungroup()
}

#' Read lines with specific encoding
#'
#' @param path Path to file
#' @param encoding Encoding to use
#'
#' @returns A character vector
#' @keywords internal
readLines_with_encoding <- function(path, encoding) {
  file_connection <- file(path, encoding = encoding)
  lines <- readLines(con = file_connection, warn = FALSE)
  close(file_connection)

  return(lines)
}

#' Get PX-file content as lines
#'
#' Read file with correct encoding.
#'
#' @param px_path Path to a PX-file
#'
#' @returns A character vector
#' @keywords internal
read_px_file <- function(px_path) {
  readLines_with_encoding(path = px_path,
                          encoding = get_encoding_from_px_file(px_path)
                          )
}

#' Read lines from file with guessed encoding
#'
#' Wrapper around readLines to guess encoding before reading.
#'
#' @param path Path to file
#'
#' @returns A character vector
#' @keywords internal
readLines_guess_encoding <- function(path) {
  readLines_with_encoding(path = path,
                          encoding = guess_file_encoding(path)
                          )
}

#' Default encoding to read and save PX-file in
#'
#' @returns Character
#' @keywords internal
get_default_encoding <- function() {
  return('latin1')
}

#' Get encoding listed in PX-file
#'
#' Encoding is listed in CODEPAGE.
#'
#' @inheritParams read_px_file
#'
#' @returns Character
#' @keywords internal
get_encoding_from_px_file <- function(px_path) {
  encoding <-
    px_path %>%
    readLines(warn = FALSE) %>%
    paste(collapse = '\n') %>%
    stringr::str_extract('(?<=CODEPAGE=").+(?=";)')

  if (is.na(encoding)) {
    encoding <- get_default_encoding()
  }

  return(encoding)
}

#' Guess encoding of file
#'
#' @param path Path to file
#' @param prefer Named list of encodings, used to give higher confidence to
#' specific encodings.
#'
#' @returns Character
#' @keywords internal
guess_file_encoding <- function(path, prefer = list("UTF-8" = .2,
                                                    "ISO-8859-1" = .2)
                                ) {

  prefer_df <-
    prefer %>%
    tibble::enframe(name = "Encoding", value = "Confidence") %>%
    tidyr::unnest("Confidence") %>%
    dplyr::mutate(across("Encoding", as.character))

  best_guess <-
    readBin(path, what = "raw", n = file.info(path)$size) %>%
    stringi::stri_enc_detect() %>%
    magrittr::extract2(1) %>%
    dplyr::bind_rows(prefer_df) %>%
    dplyr::group_by(.data$Encoding) %>%
    dplyr::summarise(Confidence = sum(.data$Confidence)) %>%
    dplyr::arrange(desc(.data$Confidence)) %>%
    .[["Encoding"]] %>%
    head(1)

  return(best_guess)
}

#' Check if a path has a specific extension (function factory)
#'
#' @param extension String, file name extension
#'
#' @returns Logic
#' @keywords internal
is_path_extension <- function(extension) {
  function(path) {
    if (is.character(path)) {
      identical(TRUE, tolower(tools::file_ext(path)) == tolower(extension))
    } else {
      FALSE
    }
  }
}

is_rds_file     <- is_path_extension("rds")
is_parquet_file <- is_path_extension("parquet")
is_xlsx_file    <- is_path_extension("xlsx")
is_px_file      <- is_path_extension("px")
is_r_file       <- is_path_extension("R")

#' Create and return path to temporary directory
#'
#' @returns Character
#' @keywords internal
temp_dir <- function() {
  path <- tempfile()
  dir.create(path)
  return(path)
}

#' Change all variables to character
#'
#' @param df Data frame
#'
#' @returns A data frame
#' @keywords internal
mutate_all_vars_to_character <- function(df) {
  dplyr::mutate(df, dplyr::across(everything(), as.character))
}

#' Create temporary file
#'
#' Get a temporary file path with a specific extension (function factory)
#'
#' @param extension String, file name extension
#'
#' @returns Path to temporary file
#' @keywords internal
temp_file_with_extension <- function(extension) {
  function() {
    return(tempfile(fileext = extension))
  }
}

temp_px_file      <- temp_file_with_extension(".px")
temp_rds_file     <- temp_file_with_extension(".rds")
temp_xlsx_file    <- temp_file_with_extension(".xlsx")
temp_r_file       <- temp_file_with_extension(".R")
temp_parquet_file <- temp_file_with_extension(".parquet")

#' Align data frames
#'
#' Align df_a to have the same columns and column types as df_b.
#'
#' @param df_a Data frame to align
#' @param df_b Data frame to align to
#'
#' @returns A data frame
#' @keywords internal
align_data_frames <- function(df_a, df_b) {
  names_a <- names(df_a)
  names_b <- names(df_b)

  # Add columns from b not in a to a
  for (names in setdiff(names_b, names_a)) {
    if (nrow(df_a) == 0) {
      df_a[[names]] <- as.character()
    } else {
      df_a[[names]] <- NA
    }
  }

  # Reorder
  df_a <- dplyr::relocate(df_a, all_of(names_b))

  # Align types
  for (name in names_b) {
    df_a[[name]] <- as(df_a[[name]], class(df_b[[name]]))
  }

  return(df_a)
}

#' Drop rows with only NA values
#'
#' @param df Data frame
#'
#' @returns A data frame
#' @keywords internal
drop_blank_rows <- function(df) {
  dplyr::filter(df, if_any(everything(), ~! is.na(.)))
}

#' Create a tibble with dummy values
#'
#' @param dummy_value Value to set in all columns
#' @keywords internal
create_dummy_tibbles <- function(dummy_value) {
  function(columns) {
    tibble::tibble(!!!setNames(rep(list(dummy_value), length(columns)), columns)
    )
  }
}

na_tibble <- create_dummy_tibbles(NA)
character0_tibble <- create_dummy_tibbles(character(0))
asterisk_tibble <- create_dummy_tibbles("*")

#' Download parquet or PX-file and return path
#'
#' Assumes the file is .px, or parquet.
#'
#' @param url
#'
#' @returns Character
#' @keywords internal
download_px_or_parquet_and_return_path <- function(url) {
  is_parquet <- stringr::str_detect(url, "parquet")

  tmp_file_path <- ifelse(is_parquet,
                          temp_parquet_file(),
                          temp_px_file()
                          )

  curl::curl_download(url, tmp_file_path, quiet = TRUE)

  return(tmp_file_path)
}
