#' Add or modify value
#'
#' Modify a value in a row of a data frame based on its value in another column.
#' If the value is not found, a new row is added.
#'
#' @param df Data frame
#' @param lookup_column Column to look up
#' @param lookup_column_values Values to look up
#' @param modify_column Column to modify
#' @param new_value New value to modify/add to modify_column
#'
#' @returns A data frame
#' @keywords internal
modify_or_add_row <- function(df,
                              lookup_column,
                              lookup_column_values,
                              modify_column,
                              new_value) {
  if (any(df[[lookup_column]] %in% lookup_column_values)) {
    df[df[[lookup_column]] %in% lookup_column_values, modify_column] <- new_value
  } else {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(!!lookup_column := lookup_column_values,
                                         !!modify_column := new_value
                           )
    )
  }

  return(df)
}

modify_or_add_in_column <- function(df,
                                    lookup_column,
                                    lookup_column_values,
                                    new_value
                                    ) {
  if (any(df[[lookup_column]] %in% lookup_column_values)) {
    df[df[[lookup_column]] %in% lookup_column_values, lookup_column] <- new_value
  } else {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(!!lookup_column := new_value)
                           )
  }

  return(df)
}

modify_with_df <- function(df1, df2, modify_column) {
  valid_names <-
    names(df1) %>%
    paste(collapse = ", ")

  invalid_columns <-
    setdiff(names(df2), names(df1)) %>%
    paste(collapse = ", ")

  if (invalid_columns != "") {
    error(stringr::str_glue(
      "Data frame contains invalid columns: ",
      "{invalid_columns}, it can only ",
      "contain the columns: {valid_names}"
    ))
  }

  merge_variables <- setdiff(names(df2), modify_column)

  df1_without_modify_column <- dplyr::select(df1, -all_of(modify_column))

  if (length(merge_variables) == 0) {
    replace_values <- dplyr::cross_join(df1_without_modify_column, df2)
    keep_values    <- dplyr::filter(df1, FALSE)
    add_values     <- dplyr::filter(df2, FALSE)
  } else {
    replace_values <- dplyr::inner_join(df1_without_modify_column, df2,
                                        by = merge_variables
                                        )
    keep_values <- dplyr::anti_join(df1, df2, by = merge_variables)
    add_values <- dplyr::anti_join(df2, df1, by = merge_variables)
  }

  return(dplyr::bind_rows(replace_values, keep_values, add_values))
}

remove_keyword_from_table <- function(table_name) {
  function(x, keyword) {
    x[[table_name]] <- x[[table_name]] %>%
      dplyr::filter(keyword != !!keyword)

    return(x)
  }
}

remove_keyword_table1 <- remove_keyword_from_table("table1")
remove_keyword_table2 <- remove_keyword_from_table("table2")

remove_keyword_variables2 <- function(x, keyword) {
  x$variables2[[tolower(keyword)]] <- NA
  return(x)
}

modify_table1 <- function(x, keyword, value) {
  x$table1 <-
    modify_or_add_row(x$table1, "keyword", keyword, "value", value) %>%
    sort_in_keyword_order()
  return(x)
}

modify_table2 <- function(x, keyword, value) {
  if (is.data.frame(value)) {
    value$keyword <- keyword
  } else if (is.character(value)) {
    value <-
      data.frame(keyword = keyword, value = value, language = defined_languages(x))
  }

  x$table2 <-
    x$table2 %>%
    modify_with_df(value, "value") %>%
    sort_in_keyword_order()

  return(x)
}

sort_in_keyword_order <- function(df) {
  df %>%
    dplyr::left_join(dplyr::select(px_keywords, "keyword", "order"),
                     by = "keyword"
                     ) %>%
    dplyr::arrange(.data$order) %>%
    dplyr::select(-"order")
}

get_cells_name <- function(number) {
  paste0("cells", number)
}

modify_cells <- function(x, number, column, value) {
  if ('values' %in% names(value)) {
    value <- dplyr::rename(value, "value" = "values")
  }

  x[[get_cells_name(number)]] <-
    modify_with_df(x[[get_cells_name(number)]], value, column) %>%
    align_data_frames(get(paste0("get_base_cells", number))()) %>%
    dplyr::arrange(match(.data$`variable-code`, names(x$data)))

  if (number == "1"){
    x$cells1 <-
      x$cells1 %>%
      dplyr::arrange(match(.data$`variable-code`, names(px_data(x))),
                     .data$order
                     )
  } else if (number == "2") {
    x$cells2 <-
      x$cells2 %>%
      dplyr::arrange(match(.data$`variable-code`, names(px_data(x))))
  } else {
    unexpected_error()
  }

  return(x)
}

modify_variables1 <- function(x, column, value) {
  x$variables1 <-
    modify_with_df(x$variables1, value, column) %>%
    dplyr::arrange(match(.data$`variable-code`, names(x$data)))

  return(x)
}

modify_variables2 <- function(x, column, value) {
  if (is.character(value)) {
    value <- dplyr::tibble("{column}" := value)
  }

  x$variables2 <-
    modify_with_df(x$variables2, value, column) %>%
    align_data_frames(get_base_variables2()) %>%
    dplyr::arrange(match(.data$`variable-code`, names(px_data(x))),
                   match(.data$language, px_languages(x))
                   )

  return(x)
}

modify_acrosscells <- function(x, value, keyword, na_to_star) {
  missing_columns <- setdiff(c(px_stub(x), px_heading(x)), names(value))

  value_completed <-
    value %>%
    { if (length(missing_columns) > 0) {
      if (na_to_star) {
        dummy_df <- asterisk_tibble(columns = missing_columns)
      } else {
        dummy_df <- na_tibble(columns = missing_columns)
      }

      dplyr::bind_cols(., dummy_df)
    } else {
      .
    }}

  x$acrosscells <-
    modify_with_df(x$acrosscells, value_completed, tolower(keyword)) %>%
    align_data_frames(get_base_acrosscells(c(px_stub(x), px_heading(x))))

  return(x)
}


get_table1_value <- function(x, keyword) {
  value <-
    x$table1 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::pull(value)

  if (identical(value, character(0))) {
    return(NULL)
  } else {
    return(value)
  }
}

get_table2_value <- function(x, keyword) {
  value <-
    x$table2 %>%
    dplyr::filter(.data$keyword == !!keyword) %>%
    dplyr::select("code", "language", "value") %>%
    { if (length(unique(.$code)) == 1) {
      dplyr::select(., -"code")
    } else {
      .
    }}

  if (nrow(value) == 0) {
    return(NULL)
  } else if (all(length(defined_languages(x)) == 1, nrow(value) == 1)) {
    return(unique(value$value))
  } else {
    return(value)
  }
}

get_cells_value <- function(x, number, column) {
  if (number == "1") {
    language_col <- NULL
  } else if (number == "2") {
    language_col <- "language"
  } else {
    unexpected_error()
  }

  x[[get_cells_name(number)]] %>%
    dplyr::select(all_of(c("variable-code", "code", language_col, !!column))) %>%
    tidyr::drop_na(!!column) %>%
    dplyr::select(where(~ ! all(is.na(.))))
}

get_variable1_value <- function(x, column) {
  value <-
    x$variables1 %>%
    dplyr::select("variable-code", !!column) %>%
    tidyr::drop_na(!!column)

  if (nrow(value) == 0) {
    return(NULL)
  } else {
    return(value)
  }
}

get_variable1_logic_value <- function(x, column) {
  value <-
    x$variables1 %>%
    dplyr::filter(.data[[column]] == TRUE) %>%
    dplyr::pull("variable-code")

  if (length(value) == 0) {
    return(NULL)
  } else {
    return(value)
  }
}

get_variables2_value <- function(x, column) {
  value <-
    x$variables2 %>%
    dplyr::select("variable-code", "language", all_of(column)) %>%
    tidyr::drop_na(all_of(column))

  if (nrow(value) == 0) {
    return(NULL)
  } else if (length(unique(x$variables2[[column]])) == 1) {
    return(unique(x$variables2[[column]]))
  } else {
    if (length(defined_languages(x)) == 1) {
      return(dplyr::select(value, -"language"))
    } else {
      return(value)
    }
  }
}

get_acrosscells_value <- function(x, keyword) {
  column_name <- tolower(keyword)

  value <-
    x$acrosscells %>%
    dplyr::select(all_of(c(intersect(names(x$acrosscells), names(x$data)),
                           "language",
                           column_name
                           )
                         )
                  )

  if (nrow(value) == 0) {
    return(NULL)
  } else if (length(defined_languages(x)) == 1) {
    return(dplyr::select(value, -"language"))
  } else {
    return(value)
  }
}
